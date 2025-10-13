# load data
library(readr)

install.packages("tidyverse")
install.packages("janitor")
install.packages("ggplot2")
library(tidyverse)
library(janitor)
library(ggplot2)

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




