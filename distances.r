# nolint start
library(readr)
library(dplyr)
library(tidyverse)
library(geosphere)

# Read in the official FBS list
fbs_list <- read_csv("./data/FBS_list.csv") %>%

  # Rename the conference column to remove the new line
  rename(conference = `Current\r\nConference`) %>%

  # Select relevant columns
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
        
# Combine the stadiums and FBS list
teams <- stadiums %>%
  left_join(fbs_list, by = c("team" = "School"))

## FBS TEAMS DO NOT PLAY AWAY AT NON-FBS TEAMS




# https://www.sports-reference.com/cfb/years/2024-schedule.html
schedule <- read_csv("./data/2024_schedule.csv") %>% 

  # Rename columns and remove weekly rankings
  mutate(
    winner = gsub("\\([0-9]{1,2}\\)\\s*", "", winner), 
    loser = gsub("\\([0-9]{1,2}\\)\\s*", "", loser),
    home = ifelse(grepl("@", away_neutral), loser, winner),
    away = ifelse(grepl("@", away_neutral), winner, loser),
    home_pts = ifelse(grepl("@", away_neutral), lose_pts, win_pts),
    away_pts = ifelse(grepl("@", away_neutral), win_pts, lose_pts)
  ) %>%

  # Correct names
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

  # Filter for games involving FBS teams
  filter(away %in% stadiums$team) %>%

  # Remove unnecessary columns and rename temporary columns
  select(-winner, -loser, -win_pts, -lose_pts)




# todo: 
# write script looping through each stadium team,
# going throguh schedule to find away games - 
# calculate disantance for each and add together and 
# assign to team

# Add a column for total mileage
teams$total_mileage <- 0

# Loop through each team and calculate the total mileage
for(i in 1:nrow(teams)){

  # Get the team and coordinates
  team <- teams$team[i]
  team_lat <- round(teams$latitude[i], digits = 4)
  team_long <- round(teams$longitude[i], digits = 4)

  # Get the away games
  away_games <- schedule[schedule$away == team, "home"]

  # reset mileage to 0
  total_mileage <- 0

  # Loop through each away game and calculate the distance
  for(j in 1:nrow(away_games)){

    # Get the coordinates of the home team
    home_team <- away_games$home[j]
    home_lat <- round(teams[teams$team == home_team, "latitude"]$latitude[1], digits = 4)
    home_long <- round(teams[teams$team == home_team, "longitude"]$longitude[1], digits = 4)

    # Calculate the distance in miles
    # not sure why it needed a matrix - had to look that one up
    distance <- distHaversine(matrix(c(team_long, team_lat), ncol = 2), matrix(c(home_long, home_lat), ncol = 2)) / 1609.34

    # Add the distance to the total mileage
    total_mileage <- total_mileage + distance
  }

  # Assign the total mileage to the team and round
  teams$total_mileage[i] <- round(total_mileage)
}

# Calculate conference mileage
conf_mileage <- teams %>%
  group_by(conference) %>%
  summarise(total_mileage = sum(total_mileage))

realinged_teams <- c("California", "SMU", "Stanford", "USC", "UCLA", "Washington", "Oregon", "Arizona", "Arizona State", "Utah", "Colorado", "Army", "Texas", "Kennesaw State", "Oklahoma")

# Add a column to indicate if the team is realigned
teams <- teams %>%
  mutate(realigned = ifelse(team %in% realinged_teams, TRUE, FALSE))




# Plot with different colors for realigned teams
p <- ggplot() +

  # create the collumn chart
  geom_col(data = teams, aes(x = reorder(team, total_mileage), y = total_mileage, fill = realigned), width = 0.4) +

  # add the text labels
  geom_text(data = teams, aes(x = reorder(team, total_mileage), y = total_mileage, label = team), hjust = -0.1, size = 2.5) +

  # add the fill colors
  scale_fill_manual(values = c(`TRUE` = "#FF6347", `FALSE` = "#4682B4"), name = "Realigned", labels = c("No", "Yes")) +

  # add the theme
  theme_minimal() +

  # adjust specifics
  theme(

    # remove axis text
    axis.text.y = element_blank(),

    # adjust axis titles
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),

    # adjust plot title
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),

    # adjust legend position and text
    legend.position = "top",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),

    # adjust background
    panel.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.background = element_rect(fill = "#F5F5F5", color = NA)

  ) +

  # add labels
  labs(x = "Team", y = "Total Mileage", title = "Total Mileage for FBS Teams") +

  # rotate the x-axis labels
  coord_flip() +

  # add dashed lines for the top 5, 10, 25, 50, and 100 teams
  geom_vline(xintercept = c(nrow(teams) - 4.5, nrow(teams) - 9.5, nrow(teams) - 24.5, nrow(teams) - 49.5, nrow(teams) - 99.5), linetype = "dashed", color = "grey")

ggsave("./out/total_mileage_fbs.png", plot = p, width = 16, height = 12, units = "in", dpi = 300)





# Remove realigned schools from each conference's calculation
conf_and_realigned_emissions <- teams %>%
  filter(!realigned) %>%
  group_by(conference) %>%
  summarise(emissions = round(sum(total_mileage) * 0.17 / n(), digits = 2))

realigned_emissions <- teams %>%
  filter(realigned) %>%
  summarise(conference = "Realigned", emissions = round(sum(total_mileage) * 0.17 / nrow(filter(teams, realigned)), digits = 2))

# Combine the two data
conf_and_realigned_emissions <- bind_rows(conf_and_realigned_emissions, realigned_emissions)

# Plot average total emissions by conference per team
p2 <- ggplot() +
  geom_col(data = conf_and_realigned_emissions, aes(x = reorder(conference, emissions), y = emissions, fill = conference), width = 0.6) +
  
  # Customize fill colors
  scale_fill_manual(values = c("Realigned" = "#FF6347"), name = "Conference") +
  
  theme_minimal() +
  
  # Adjust theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.background = element_rect(fill = "#F5F5F5", color = NA)
  ) +
  
  # Add labels
  labs(x = "Conference", y = "Total Emissions (kg)", title = "Average Total Emissions by Conference Member")

ggsave("./out/total_emissions_by_conference.png", plot = p2, width = 16, height = 12, units = "in", dpi = 300)
# nolint end