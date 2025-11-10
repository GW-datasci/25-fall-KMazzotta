# load data
library(readr)

install.packages("tidyverse")
install.packages("janitor")
install.packages("ggplot2")
install.packages("lubridate")
library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)
library(lubridate)

ultras <- read_csv("ultras.csv") %>% 
  clean_names() %>%
  filter(
         !is.na(athlete_club) &
         !is.na(athlete_year_of_birth) &
         !is.na(athlete_age_category) &
         str_detect(event_name, "USA")) %>%
  mutate(athlete_performance = hms(str_replace_all(athlete_performance, "h", "")))

summary(ultras)

ultras$age <- ultras$year_of_event - ultras$athlete_year_of_birth


ultras <- ultras %>%
  group_by(age) %>%
  filter(age > 18) %>%
  ungroup()

summary(ultras$age)
min(ultras$age)

problems(ultras)

# some basic plots to start
ggplot(ultras, aes(x = athlete_gender)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Number of Athletes by Gender",
    x = "Gender",
    y = "Count"
  ) +
  theme_minimal()

ggplot(ultras, aes(x = athlete_age_category)) +
  geom_bar(fill = "darkseagreen") +
  labs(
    title = "Distribution of Athletes by Age Category",
    x = "Age Category",
    y = "Count"
  ) +
  theme_minimal() +
  coord_flip()



early_regression <- lm()





make a 100 mile dataset


make a 50 mile dataset

run same regressions for both and compare





# 50km data
fifty_k_data <- ultras %>%
  filter(event_distance_length %in% c("50km", "33mi")) %>%
  group_by(event_number_of_finishers) %>%
  filter(event_number_of_finishers > 200) %>%
  ungroup() %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

# 50mi data
fifty_mi_data <- ultras %>%
  filter(event_distance_length == "50mi") %>%
  group_by(event_number_of_finishers) %>%
  filter(event_number_of_finishers > 200) %>%
  ungroup() %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

# 100km data
hundred_k_data <- ultras %>%
  filter(event_distance_length == "100km") %>%
  group_by(event_number_of_finishers) %>%
  filter(event_number_of_finishers > 200) %>%
  ungroup() %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

# 100mi data
hundred_mi_data <- ultras %>%
  filter(event_distance_length == "100mi") %>%
  group_by(event_number_of_finishers) %>%
  filter(event_number_of_finishers > 200) %>%
  ungroup() %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()


# looking for the average number of participants in 50k data
fifty_k_data <- ultras %>%
  filter(event_distance_length %in% c("50km", "33mi")) %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

avg_50k_partic <- nrow(fifty_k_data) / length(unique(fifty_k_data$event_name))
avg_50k_partic 
# = 227.39

# looking for the average number of participants in 50mi data
fifty_mi_data <- ultras %>%
  filter(event_distance_length == "50mi") %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

avg_50mi_partic <- nrow(fifty_mi_data) / length(unique(fifty_mi_data$event_name))
avg_50mi_partic 
# = 210.82

# looking for the average number of participants in 100k data
hundred_k_data <- ultras %>%
  filter(event_distance_length == "100km") %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

avg_100k_partic <- nrow(hundred_k_data) / length(unique(hundred_k_data$event_name))
avg_100k_partic 
# = 126.48

# looking for the average number of participants in 100mi data
hundred_mi_data <- ultras %>%
  filter(event_distance_length == "100mi") %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

avg_100mi_partic <- nrow(hundred_mi_data) / length(unique(hundred_mi_data$event_name))
avg_100mi_partic 
# = 183.49

# average of the average race participants
avg_avg <- (avg_50k_partic+avg_50mi_partic+avg_100k_partic+avg_100mi_partic)/4
avg_avg
# = 187.05



# for regression split the data for junior exploration
# train and test 

#70% for train
#30% for test after the exploration is done. makr the swithc at the end before mating the report


hundred_k_sample <- hundred_k_data %>%
  sample_n(size = 23604*.3, replace = FALSE)

plot(hundred_k_sample$athlete_average_speed, hundred_k_sample$event_number_of_finishers)

# make a column for athlete age at time of race

# save plots as png
ggsave


# use overleaf

ggplot(hundred_k_sample, aes(x = athlete_country)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Athlete Country", y = "Count", title = "Number of Athletes per Country (100K)")

hundred_k_sample %>%
  distinct(event_name, athlete_country) %>%
  ggplot(aes(x = athlete_country)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Country", y = "Number of Events", title = "Events per Country (100K)")

summary(hundred_k_data)

fifty_k_sample <- fifty_k_data %>%
  sample_n(size = 243502*.01, replace = FALSE) %>%
  mutate(athlete_performance = as.numeric(athlete_performance, "hours"))

fifty_mi_sample <- fifty_mi_data %>%
  sample_n(size = 84274*.01, replace = FALSE) %>%
  mutate(athlete_performance = as.numeric(athlete_performance, "hours"))

hundred_k_sample <- hundred_k_data %>%
  sample_n(size = 23568*.01, replace = FALSE) %>%
  mutate(athlete_performance = as.numeric(athlete_performance, "hours"))

hundred_mi_sample <- hundred_mi_data %>%
  sample_n(size = 47691*.01, replace = FALSE) %>%
  mutate(athlete_performance = as.numeric(athlete_performance, "hours"))

plot(fifty_k_sample$age, fifty_k_sample$athlete_average_speed)


prelim_50k_regression <- lm(athlete_performance ~ event_number_of_finishers + athlete_gender, data = fifty_k_sample)
prelim_50mi_regression <- lm(athlete_performance ~ event_number_of_finishers + athlete_gender, data = fifty_mi_sample)
prelim_100k_regression <- lm(athlete_performance ~ event_number_of_finishers + athlete_gender, data = hundred_k_sample)
prelim_100mi_regression <- lm(athlete_performance ~ event_number_of_finishers + athlete_gender, data = hundred_mi_sample)


prelim_50k_regression

summary(prelim_50k_regression)
summary(prelim_100k_regression)
summary(prelim_50mi_regression)
summary(prelim_100mi_regression)

anova(prelim_50k_regression, prelim_50mi_regression)



  
