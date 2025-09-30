# load data
library(readr)

install.packages("tidyverse")
install.packages("janitor")
library(janitor)
ultras <- read_csv("ultras.csv") %>% 
  clean_names() %>%
  filter(
         !is.na(athlete_club) &
         !is.na(athlete_year_of_birth) &
         !is.na(athlete_age_category) &
         str_detect(event_name, "USA"))



summary(ultras)




