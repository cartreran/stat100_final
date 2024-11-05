library(readr)
library(dplyr)
library(tidyverse)
library(geosphere)
library(ggplot2)
library(giscoR)
library(ggrepel)

# Read in the official FBS list
fbs_list <- read_csv("./data/FBS_list.csv") %>%
  rename(conference = `Current\r\nConference`) %>%
  select(School, conference) %>%
  mutate(
    School = recode(School, "Louisiana–Monroe" = "Louisiana-Monroe")
  )

# Read in the stadiums data
stadiums <- read_csv("./data/stadiums-geocoded.csv") %>%
  select(team, capacity, div, latitude, longitude) %>%
  filter(div == "fbs" & team != "Idaho") %>%
  add_row(team = "Coastal Carolina", capacity = 21000, div = "fbs", latitude = 33.7930, longitude = -79.0177) %>%
  add_row(team = "Jacksonville State", capacity = 24000, div = "fbs", latitude = 33.8203, longitude = -85.7664) %>%
  add_row(team = "James Madison", capacity = 25000, div = "fbs", latitude = 38.4344, longitude = -78.8704) %>%
  add_row(team = "Kennesaw State", capacity = 10200, div = "fbs", latitude = 34.0290, longitude = -84.5676) %>%
  add_row(team = "Liberty", capacity = 25000, div = "fbs", latitude = 37.3523, longitude = -79.1716) %>%
  add_row(team = "Sam Houston", capacity = 14000, div = "fbs", latitude = 30.7083, longitude = -95.5383) %>%
  add_row(team = "UAB", capacity = 47100, div = "fbs", latitude = 33.4971, longitude = -86.8121) %>%
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

# Read in the schedule data
schedule <- read_csv("./data/2024_schedule.csv") %>%
  mutate(
    winner = gsub("\\([0-9]{1,2}\\)\\s*", "", winner),
    loser = gsub("\\([0-9]{1,2}\\)\\s*", "", loser),
    home = ifelse(grepl("@", away_neutral), loser, winner),
    away = ifelse(grepl("@", away_neutral), winner, loser),
    home_pts = ifelse(grepl("@", away_neutral), lose_pts, win_pts),
    away_pts = ifelse(grepl("@", away_neutral), win_pts, lose_pts)
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
  filter(away %in% stadiums$team) %>%
  select(-winner, -loser, -win_pts, -lose_pts)

# Add a column for total mileage
teams$total_mileage <- 0

# Loop through each team and calculate the total mileage
for (i in 1:nrow(teams)) {
  team <- teams$team[i]
  team_lat <- round(teams$latitude[i], digits = 4)
  team_long <- round(teams$longitude[i], digits = 4)
  away_games <- schedule[schedule$away == team, "home"]
  total_mileage <- 0

  for (j in 1:nrow(away_games)) {
    home_team <- away_games$home[j]
    home_lat <- round(teams[teams$team == home_team, "latitude"]$latitude[1], digits = 4)
    home_long <- round(teams[teams$team == home_team, "longitude"]$longitude[1], digits = 4)
    distance <- distHaversine(matrix(c(team_long, team_lat), ncol = 2), matrix(c(home_long, home_lat), ncol = 2)) / 1609.34
    total_mileage <- total_mileage + distance
  }

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
  geom_col(data = teams, aes(x = reorder(team, total_mileage), y = total_mileage, fill = realigned), width = 0.4) +
  geom_text(data = teams, aes(x = reorder(team, total_mileage), y = total_mileage, label = team), hjust = -0.1, size = 2.5) +
  scale_fill_manual(values = c(`TRUE` = "#FF6347", `FALSE` = "grey"), name = "Realigned", labels = c("No", "Yes")) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.background = element_rect(fill = "#F5F5F5", color = NA)
  ) +
  labs(x = "Team", y = "Total Mileage", title = "Total Mileage for FBS Teams") +
  coord_flip() +
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
  scale_fill_manual(values = c("Realigned" = "#FF6347"), name = "Conference") +
  theme_minimal() +
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
  labs(x = "Conference", y = "Total Emissions (kg)", title = "Average Total Emissions by Conference Member")

ggsave("./out/total_emissions_by_conference.png", plot = p2, width = 16, height = 12, units = "in", dpi = 300)

US <- gisco_get_countries(country = "US", resolution = "1")

breakpoints <- c(2000, 3000, 4000, 5000, 6000, 7000)
p3 <- ggplot() +
  geom_sf(data = US, fill = "lightgrey", color = "white", alpha = 0.5) +
  geom_point(data = teams, aes(x = longitude, y = latitude, size = total_mileage, color = realigned), alpha = 0.8) +
  theme_void() +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  scale_size_continuous(name = "Total Distance (mi)", breaks = breakpoints, range = c(2, 10)) +
  labs(title = "Total Mileage for FBS Teams Across the US") +
  scale_color_manual(values = c(`TRUE` = "#FF6347", `FALSE` = "grey"), name = "Realigned", labels = c("No", "Yes")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.box.just = "left",
    panel.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.background = element_rect(fill = "#F5F5F5", color = NA)
  ) +
  geom_text_repel(
    data = teams %>% filter(realigned), aes(x = longitude, y = latitude, label = team), size = 5,
    box.padding = 0.35, point.padding = 0.5, segment.color = "grey50"
  )

ggsave("./out/total_mileage_map.png", plot = p3, width = 16, height = 12, units = "in", dpi = 300)
