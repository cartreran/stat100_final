#nolint start
library(readr)
library(dplyr)
library(tidyverse)
library(geosphere)
library(ggplot2)
library(giscoR)
library(ggrepel)
library(ggridges)
source("./R/clean_schedule.r")

realinged_teams <- c("California", "SMU", "Stanford", "USC", "UCLA", "Washington", "Oregon", "Arizona", "Arizona State", "Utah", "Colorado", "Army", "Texas", "Kennesaw State", "Oklahoma")

US <- gisco_get_countries(country = "US", resolution = "1")

breakpoints <- c(2000, 3000, 4000, 5000, 6000, 7000)

fbs_list <- read_csv("./data/FBS_list.csv") %>%
  rename(conference = `Current\r\nConference`) %>%
  select(School, conference) %>%
  mutate(School = recode(School, "Louisiana–Monroe" = "Louisiana-Monroe"))

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
  mutate(team = recode(team,
    "Louisiana-Lafayette" = "Louisiana",
    "Connecticut" = "UConn",
    "Louisiana-Monroe" = "Louisiana-Monroe",
    "Miami" = "Miami (FL)",
    "NIU" = "Northern Illinois",
    "Mississippi" = "Ole Miss",
    "USF" = "South Florida",
    "Southern California" = "USC"
  ))

teams <- stadiums %>%
  left_join(fbs_list, by = c("team" = "School"))

dat_2021 <- clean(read_csv("./data/2021_schedule.csv") %>% rename_all(tolower), stadiums)
dat_2022 <- clean(read_csv("./data/2022_schedule.csv") %>% rename_all(tolower), stadiums)
dat_2023 <- clean(read_csv("./data/2023_schedule.csv") %>% rename_all(tolower), stadiums)
dat_2024 <- clean(read_csv("./data/2024_schedule.csv") %>% rename_all(tolower), stadiums)

schedules <- list(
  "2021" = dat_2021,
  "2022" = dat_2022,
  "2023" = dat_2023,
  "2024" = dat_2024
)

get_distance <- function(year, teams) {
  for (i in 1:nrow(teams)) {
    total_mileage <- 0
    team <- teams$team[i]
    team_lat <- round(teams$latitude[i], digits = 4)
    team_long <- round(teams$longitude[i], digits = 4)
    away_games <- schedules[[year]][schedules[[year]]$away == team, "home"]
    for (j in 1:nrow(away_games)) {
      home_team <- away_games$home[j]
      if (!is.na(home_team) && any(teams$team == home_team)) {
        home_lat <- round(teams[teams$team == home_team, "latitude"]$latitude[1], digits = 4)
        home_long <- round(teams[teams$team == home_team, "longitude"]$longitude[1], digits = 4)
        distance <- distHaversine(matrix(c(team_long, team_lat), ncol = 2), matrix(c(home_long, home_lat), ncol = 2)) / 1609.34
        total_mileage <- total_mileage + distance
      }
    }
    total_mileage <- round(total_mileage)
    col_name <- paste0("total_mileage_", year)
    teams[[col_name]][i] <- 0
    teams[[col_name]][i] <- total_mileage
  }
  return(teams)
}

teams <- get_distance("2021", teams)
teams <- get_distance("2022", teams)
teams <- get_distance("2023", teams)
teams <- get_distance("2024", teams)

conf_mileage <- teams %>%
  group_by(conference) %>%
  summarise(total_mileage = sum(total_mileage_2024))

teams <- teams %>%
  mutate(realigned = ifelse(team %in% realinged_teams, TRUE, FALSE))

conf_and_realigned <- bind_rows(teams %>% filter(!realigned & conference != "Pac-12"), teams %>% filter(realigned) %>% mutate(conference = "Realigned"))

teams_long <- teams %>%
  select(team, total_mileage_2021, total_mileage_2022, total_mileage_2023, total_mileage_2024, realigned) %>%
  pivot_longer(
    cols = starts_with("total_mileage"),
    names_to = "year",
    values_to = "mileage"
  ) %>%
  mutate(year = as.numeric(str_extract(year, "\\d{4}")))


conf_and_realigned_emissions <- teams %>%
  filter(!realigned) %>%
  group_by(conference) %>%
  summarise(emissions = round(sum(total_mileage_2024) * 0.17 / n(), digits = 2))

realigned_emissions <- teams %>%
  filter(realigned) %>%
  summarise(conference = "Realigned", emissions = round(sum(total_mileage_2024) * 0.17 / nrow(filter(teams, realigned)), digits = 2))

conf_and_realigned_emissions <- bind_rows(conf_and_realigned_emissions, realigned_emissions)

p <- ggplot() +
  geom_col(data = teams, aes(x = reorder(team, total_mileage_2024), y = total_mileage_2024, fill = realigned), width = 0.4) +
  geom_text(data = teams, aes(x = reorder(team, total_mileage_2024), y = total_mileage_2024, label = team), hjust = -0.1, size = 2.5) +
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

p3 <- ggplot() +
  geom_sf(data = US, fill = "lightgrey", color = "white", alpha = 0.5) +
  geom_point(data = teams, aes(x = longitude, y = latitude, size = total_mileage_2024, color = realigned), alpha = 0.8) +
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

p4 <- ggplot(teams_long, aes(x = year, y = mileage, group = team, color = realigned)) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = 2024, linetype = "dashed", color = "black", alpha = 0.5) +
      annotate("text", x = 2024, y = max(teams_long$mileage), 
           label = "Conference\nRealignment", 
           hjust = 1.1, vjust = 1) +
      scale_color_manual(values = c(`TRUE` = "#FF6347", `FALSE` = "grey"), 
                name = "Realigned", labels = c("No", "Yes")) +
      theme_minimal() +
      labs(
        title = "Change in Travel Distance Over Time (2021-2024)",
        x = "Year",
        y = "Total Mileage"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "top",
        panel.background = element_rect(fill = "#F5F5F5", color = NA),
        plot.background = element_rect(fill = "#F5F5F5", color = NA)
      )

ggsave("./out/total_mileage_fbs.png", plot = p, width = 16, height = 12, units = "in", dpi = 300)
ggsave("./out/total_emissions_by_conference.png", plot = p2, width = 16, height = 12, units = "in", dpi = 300)
ggsave("./out/total_mileage_map.png", plot = p3, width = 16, height = 12, units = "in", dpi = 300)
ggsave("./out/mileage_over_years_line.png", plot = p4, width = 16, height = 12, units = "in", dpi = 300)
