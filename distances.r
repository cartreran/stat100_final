library(readr)
library(dplyr)
library(tidyverse)
library(plotly)
library(gapminder)

# Read in the offical FBS list
fbs_list <- read_csv("./data/FBS_list.csv")

# Read in the stadiums data
# https://github.com/gboeing/data-visualization/tree/main/ncaa-football-stadiums/data
stadiums <- read_csv("./data/stadiums-geocoded.csv") %>%
  select(team, conference, capacity, div, latitude, longitude) %>%
  filter(div == "fbs" & team != "Idaho") %>%
  add_row(team = "Coastal Carolina", conference = NA, capacity = NA, div = "fbs", latitude = NA, longitude = NA) %>%
  add_row(team = "Jacksonville State", conference = NA, capacity = NA, div = "fbs", latitude = NA, longitude = NA) %>%
  add_row(team = "James Madison", conference = NA, capacity = NA, div = "fbs", latitude = NA, longitude = NA) %>%
  add_row(team = "Kennesaw State", conference = NA, capacity = NA, div = "fbs", latitude = NA, longitude = NA) %>%
  add_row(team = "Liberty", conference = NA, capacity = NA, div = "fbs", latitude = NA, longitude = NA) %>%
  add_row(team = "Sam Houston", conference = NA, capacity = NA, div = "fbs", latitude = NA, longitude = NA) %>%
  add_row(team = "UAB", conference = NA, capacity = NA, div = "fbs", latitude = NA, longitude = NA)
        
# Load the schedule dataset
# https://www.sports-reference.com/cfb/years/2024-schedule.html
schedule <- read_csv("./data/2024_schedule.csv") %>% 
  mutate(
    home = gsub("\\([0-9]{1,2}\\)\\s*", "", winner), 
    away = gsub("\\([0-9]{1,2}\\)\\s*", "", loser),
    home_pts = win_pts,
    away_pts = lose_pts
  ) %>%
  mutate(
    temp_home = ifelse(grepl("@", away_neutral), away, home),
    temp_away = ifelse(grepl("@", away_neutral), home, away),
    temp_home_pts = ifelse(grepl("@", away_neutral), away_pts, home_pts),
    temp_away_pts = ifelse(grepl("@", away_neutral), home_pts, away_pts)
  ) %>%
  filter(temp_away %in% stadiums$team) %>%
  select(-home, -away, -home_pts, -away_pts, -winner, -loser, -win_pts, -lose_pts) %>%
  rename(
    home = temp_home,
    away = temp_away,
    home_pts = temp_home_pts,
    away_pts = temp_away_pts
  )