library(readr)
library(dplyr)
library(tidyverse)
library(plotly)
library(gapminder)

# Read in the official FBS list
fbs_list <- read_csv("./data/FBS_list.csv") %>%
  rename(conference = `Current\r\nConference`) %>%
  select(School, conference) %>%
  mutate(
    # Correct the name of the school
    School = recode(School,
    "Louisiana–Monroe" = "Louisiana-Monroe")
  )

# Read in the stadiums data
# Source: https://github.com/gboeing/data-visualization/tree/main/ncaa-football-stadiums/data
stadiums <- read_csv("./data/stadiums-geocoded.csv") %>%
  # Select relevant columns
  select(team, capacity, div, latitude, longitude) %>%
  # Filter for FBS teams and exclude Idaho
  filter(div == "fbs" & team != "Idaho") %>%
  # Add rows for new FBS teams
  add_row(team = "Coastal Carolina", capacity = 21000, div = "fbs", latitude = 33.7930, longitude = -79.0177) %>%
  add_row(team = "Jacksonville State", capacity = 24000, div = "fbs", latitude = 33.8203, longitude = -85.7664) %>%
  add_row(team = "James Madison", capacity = 25000, div = "fbs", latitude = 38.4344, longitude = -78.8704) %>%
  add_row(team = "Kennesaw State", capacity = 10200, div = "fbs", latitude = 34.0290, longitude = -84.5676) %>%
  add_row(team = "Liberty", capacity = 25000, div = "fbs", latitude = 37.3523, longitude = -79.1716) %>%
  add_row(team = "Sam Houston", capacity = 14000, div = "fbs", latitude = 30.7083, longitude = -95.5383) %>%
  add_row(team = "UAB", capacity = 47100, div = "fbs", latitude = 33.4971, longitude = -86.8121) %>%
  # Correct team names
  mutate(
     team = recode(team,
      "Louisiana-Lafayette" = "Louisiana",
      "Connecticut" = "UConn",
      "Louisiana-Monroe" = "Louisiana-Monroe",
      "Miami" = "Miami (FL)",
      "NIU" = "Northern Illinois",
      "Mississippi" = "Ole Miss",
      "USF" = "South Florida",
      "Southern California" = "USC"
    )
  )
        
stadiums <- stadiums %>%
  left_join(fbs_list, by = c("team" = "School"))

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
    home = recode(home,
      "Connecticut" = "UConn",
      "North Carolina State" = "NC State",
      "Southern California" = "USC",
      "Texas-San Antonio" = "UTSA",
      "Texas-El Paso" = "UTEP",
      "Texas Christian" = "TCU",
      "Brigham Young" = "BYU",
      "Central Florida" = "UCF",
      "Southern Methodist" = "SMU",
      "Mississippi" = "Ole Miss",
      "Florida International" = "FIU",
      "Nevada-Las Vegas" = "UNLV",
      "Middle Tennessee State" = "Middle Tennessee",
      "Massachusetts" = "UMass",
      "Louisiana State" = "LSU",
      "Southern Mississippi" = "Southern Miss",
      "Alabama-Birmingham" = "UAB"
    ),
    away = recode(away,
      "Connecticut" = "UConn",
      "North Carolina State" = "NC State",
      "Southern California" = "USC",
      "Texas-San Antonio" = "UTSA",
      "Texas-El Paso" = "UTEP",
      "Texas Christian" = "TCU",
      "Brigham Young" = "BYU",
      "Central Florida" = "UCF",
      "Southern Methodist" = "SMU",
      "Mississippi" = "Ole Miss",
      "Florida International" = "FIU",
      "Nevada-Las Vegas" = "UNLV",
      "Middle Tennessee State" = "Middle Tennessee",
      "Massachusetts" = "UMass",
      "Louisiana State" = "LSU",
      "Southern Mississippi" = "Southern Miss",
      "Alabama-Birmingham" = "UAB"
    )
  ) %>%
  # Adjust for neutral site games
  mutate(
    temp_home = ifelse(grepl("@", away_neutral), away, home),
    temp_away = ifelse(grepl("@", away_neutral), home, away),
    temp_home_pts = ifelse(grepl("@", away_neutral), away_pts, home_pts),
    temp_away_pts = ifelse(grepl("@", away_neutral), home_pts, away_pts)
  ) %>%
  # Filter for games involving FBS teams
  filter(temp_away %in% stadiums$team) %>%
  # Remove unnecessary columns and rename temporary columns
  select(-home, -away, -home_pts, -away_pts, -winner, -loser, -win_pts, -lose_pts) %>%
  rename(
    home = temp_home,
    away = temp_away,
    home_pts = temp_home_pts,
    away_pts = temp_away_pts
  )
