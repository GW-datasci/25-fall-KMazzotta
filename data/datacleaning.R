# load data
library(readr)

install.packages("tidyverse")
install.packages("janitor")
install.packages("ggplot2")
library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)

ultras <- read_csv("ultras.csv") %>% 
  clean_names() %>%
  filter(
         !is.na(athlete_club) &
         !is.na(athlete_year_of_birth) &
         !is.na(athlete_age_category) &
         str_detect(event_name, "USA"))

summary(ultras)


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
  group_by(event_name) %>%
  filter(n() > 200) %>%
  ungroup() %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

# 50mi data
fifty_mi_data <- ultras %>%
  filter(event_distance_length == "50mi") %>%
  group_by(event_name) %>%
  filter(n() > 200) %>%
  ungroup() %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

# 100km data
hundred_k_data <- ultras %>%
  filter(event_distance_length == "100km") %>%
  group_by(event_name) %>%
  filter(n() > 200) %>%
  ungroup() %>%
  group_by(year_of_event) %>%
  filter(year_of_event >= 2015) %>%
  ungroup()

# 100mi data
hundred_mi_data <- ultras %>%
  filter(event_distance_length == "100mi") %>%
  group_by(event_name) %>%
  filter(n() > 200) %>%
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

