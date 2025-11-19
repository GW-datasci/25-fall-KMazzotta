
install.packages("jsonlite")
library("jsonlite")

json_data <- fromJSON(paste(readLines("utmb-race-data-raw.json"), collapse=""))


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

# only races with 200+ participants
us_races_filtered <- us_races[
  sapply(us_races, function(x) {
    length(x$Results) > 200
  })
]

# categorize distances to then create separate datasets





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




