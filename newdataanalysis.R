
install.packages("jsonlite")
library("jsonlite")

#json_data <- fromJSON(paste(readLines("utmb-race-data-raw.json"), collapse=""))



#us_races <- fromJSON(paste(readLines("us_races.json"), collapse=""))





df <- do.call(rbind, lapply(trial_ultra, as.data.frame))  

library(dplyr)
df <- bind_rows(trial_ultra) 

trial_ultra <- fromJSON("utmb-race-data-raw.json", flatten=TRUE)
colnames(json_data)
str(trial_ultra, max.level = 1)

str(trial_ultra[[1]])


# only US races
unique(names(trial_ultra[[1]]$`City / Country`))
names(trial_ultra[[1]])
us_races <- trial_ultra[
  sapply(trial_ultra, function(x) {
    grepl("United States|USA|US$", x$`City / Country`, ignore.case = TRUE)
  })
]

#maybe disregard this aspect
# only races with 200+ participants
us_races_filtered <- us_races[
  sapply(us_races, function(x) {
    length(x$Results) > 200
  })
]

# categorize distances to then create separate datasets




#writing a us_races json file
write_json(
  us_races, 
  path = "us_races.json", 
  pretty = FALSE,      # no indentation = smaller file
  auto_unbox = TRUE    # single-element vectors are written as scalars
)


# flatten and create a df by race (each row is a race)
library(dplyr)
library(purrr)
library(jsonlite)
library(stringr)


race_df <- imap_dfr(us_races, ~{
  results <- .x$Results
  tibble(
    race_id       = .y,
    city_country  = .x$`City / Country`,
    date          = .x$Date,
    distance      = .x$Distance,
    elevation_gain= .x$`Elevation Gain`,
    n_participants= length(results),
    mean_time     = mean(results, na.rm = TRUE),
    median_time   = median(results, na.rm = TRUE),
    sd_time       = sd(results, na.rm = TRUE),
    min_time      = min(results, na.rm = TRUE),
    max_time      = max(results, na.rm = TRUE),
    pct_women     = .x$Sex$Women / sum(unlist(.x$Sex)),
    pct_age_20_34 = .x$Age$`20-34` / sum(unlist(.x$Age))
  )
})


test_df <- imap_dfr(us_races, ~{
  results <- .x$Results
  tibble(
    race_id       = .y,
    city_country  = .x$`City / Country`,
    date          = .x$Date,
    distance      = .x$Distance,
    elevation_gain= .x$`Elevation Gain`,
    n_participants= length(results),
    mean_time     = mean(results, na.rm = TRUE),
    median_time   = median(results, na.rm = TRUE),
    sd_time       = sd(results, na.rm = TRUE),
    min_time      = min(results, na.rm = TRUE),
    max_time      = max(results, na.rm = TRUE),
    pct_women     = .x$Sex$Women / sum(unlist(.x$Sex)),
    pct_age_20_34 = .x$Age$`20-34` / sum(unlist(.x$Age)),
    pct_age_35_39 = .x$Age$`35-39` / sum(unlist(.x$Age)),
    pct_age_40_44 = .x$Age$`40-44` / sum(unlist(.x$Age)),
    pct_age_45_49 = .x$Age$`45-49` / sum(unlist(.x$Age)),
    pct_age_50_54 = .x$Age$`50-54` / sum(unlist(.x$Age)),
    pct_age_55_59 = .x$Age$`55-59` / sum(unlist(.x$Age)),
    pct_age_60_64 = .x$Age$`55-59` / sum(unlist(.x$Age)),
    pct_age_65_69 = .x$Age$`55-59` / sum(unlist(.x$Age)),
    pct_age_70_74 = .x$Age$`55-59` / sum(unlist(.x$Age)),
    pct_age_u18   = .x$Age$`U18`|`U20` / sum(unlist(.x$Age))
  )
})

#filter distance races
race_df$distance <- gsub("KM", "", race_df$distance)
colnames(race_df)[4] <- "distance_km"
colnames(race_df)[5] <- "elevation_gain_m"
race_df <- race_df %>%
  mutate(
    distance_km = as.numeric(distance_km)
  ) %>%
  mutate(
    elevation_gain_m = as.numeric(str_replace(elevation_gain_m, " M\\+", ""))
  )

# flatten and create a df on participants (each row is a participant)








# some modeling with 50k race_df

# select only 50k
fifty_k_data <- race_df %>%
  filter(between(distance_km, 47, 53)) %>%
  group_by(n_participants) %>%
  filter(n_participants >= 200) %>%
  ungroup()

prelim_50k_regression <- lm(mean_time ~ pct_women + pct_age_20_34 + elevation_gain_m, data = fifty_k_data)
summary(prelim_50k_regression)
plot(prelim_50k_regression, which=2, main="50k", pch=20)
plot(prelim_50k_regression, which=1, main="50k", pch=20)

#stepwise regression using backwards elimination
full_50k_regression <- lm(mean_time ~ pct_women * pct_age_20_34 * elevation_gain_m, data = fifty_k_data)
summary(full_50k_regression)
#remove pct_women
first_50k_regression <- lm(mean_time ~ pct_age_20_34 * elevation_gain_m, data = fifty_k_data)
summary(first_50k_regression)
plot(first_50k_regression, which=2, main="50k", pch=20)
plot(first_50k_regression, which=1, main="50k", pch=20)




#using test_df with age data
# select only 50k
test_df$distance <- gsub("KM", "", test_df$distance)
colnames(test_df)[4] <- "distance_km"
colnames(test_df)[5] <- "elevation_gain_m"
test_df <- test_df %>%
  mutate(
    distance_km = as.numeric(distance_km)
  ) %>%
  mutate(
    elevation_gain_m = as.numeric(str_replace(elevation_gain_m, " M\\+", ""))
  )

test_fifty_k_data <- test_df %>%
  filter(between(distance_km, 47, 53)) %>%
  group_by(n_participants) %>%
  filter(n_participants >= 200) %>%
  ungroup()
#stepwise regression using backwards elimination
install.packages("MASS")
library("MASS")


test_full_50k_regression <- lm(
  mean_time ~ elevation_gain_m + pct_women +
    pct_age_35_39 + pct_age_40_44 + pct_age_45_49 +
    pct_age_50_54 + pct_age_55_59 + pct_age_60_64 +
    pct_age_65_69 + pct_age_70_74 + pct_age_u18 +
    elevation_gain_m:(pct_women + pct_age_35_39 + pct_age_40_44 + 
                        pct_age_45_49 + pct_age_50_54 + pct_age_55_59 +
                        pct_age_60_64 + pct_age_65_69 + pct_age_70_74 + pct_age_u18) +
    pct_women:(pct_age_35_39 + pct_age_40_44)
  ,
  data = test_fifty_k_data
)
plot(test_full_50k_regression, which=2, main="50k", pch=20)
plot(test_full_50k_regression, which=1, main="50k", pch=20)

backward_step_model <- stepAIC(test_full_50k_regression, direction = "backward")
both_step_model <- stepAIC(test_full_50k_regression, direction = "both")
summary(backward_step_model)
summary(both_step_model)
summary(test_full_50k_regression)

plot(backward_step_model, which=2, main="50k", pch=20)
plot(backward_step_model, which=1, main="50k", pch=20)

#check vif
install.packages("car")
library("car")
vif(backward_step_model, type = "predictor")


#basic model using only the main effects 9of pct_women, elevation, and age bins
basic_50k_regression <- lm(
  mean_time ~ elevation_gain_m + pct_women +
    pct_age_35_39 + pct_age_40_44 + pct_age_45_49 +
    pct_age_50_54 + pct_age_55_59 + pct_age_60_64 +
    pct_age_65_69 + pct_age_70_74 + pct_age_u18
  ,
  data = test_fifty_k_data
)
summary(basic_50k_regression)
plot(basic_50k_regression, which=2, main="50k", pch=20)
plot(basic_50k_regression, which=1, main="50k", pch=20)

#check cooks distance for outliers in backward model
library(("ols"))
plot(backward_step_model, which = 4)
#no cooks d greater than 0.5





#looking at distance as a quantitative continuous variable
#using us races only but may move to all countries
distance_in_regression <- lm(
  mean_time ~ elevation_gain_m + distance_km + pct_women +
    pct_age_35_39 + pct_age_40_44 + pct_age_45_49 +
    pct_age_50_54 + pct_age_55_59 + pct_age_60_64 +
    pct_age_65_69 + pct_age_70_74 + pct_age_u18 +
    elevation_gain_m:(distance_km + pct_women + pct_age_35_39 + pct_age_40_44 + 
                        pct_age_45_49 + pct_age_50_54 + pct_age_55_59 +
                        pct_age_60_64 + pct_age_65_69 + pct_age_70_74 + pct_age_u18) +
    pct_women*distance_km,
  data = test_df
)
plot(distance_in_regression, which=2, main="50k", pch=20)
plot(distance_in_regression, which=1, main="50k", pch=20)

distance_backward_step_model <- stepAIC(distance_in_regression, direction = "backward")
summary(distance_backward_step_model)
