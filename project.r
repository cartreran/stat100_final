#nolint start

## Load libraries
library(readr)
library(dplyr)
library(tidyverse)
library(geosphere)
library(ggplot2)
library(giscoR)
library(ggrepel)
library(ggridges)
library(gdtools)

## Load Functions
source("./R/functions.r")

## Create list of schools that realinged
realinged_teams <- c("California", "SMU", "Stanford", "USC", "UCLA", "Washington", "Oregon", "Arizona", "Arizona State", "Utah", "Colorado", "Army", "Texas", "Kennesaw State", "Oklahoma")

## Gets US Map
US <- gisco_get_countries(country = "US", resolution = "1")

## Assings Time Zone variable 
time_zones <- data.frame(
  lon = c( -82, -97, -114),
  label = c( "Central", "Mountain", "Pacific")
)

## Breakpoints for circles
breakpoints <- c(2000, 3000, 4000, 5000, 6000, 7000)

## Reads official list of wikipedia data, removes new lines, and renames ULM
fbs_list <- read_csv("./data/FBS_list.csv") %>%
  rename(conference = `Current\r\nConference`) %>%
  select(School, conference) %>%
  mutate(School = recode(School, "Louisiana–Monroe" = "Louisiana-Monroe"))

## Reads stadium data, selects correct teams, excludes Idaho, adds missing teams, standardizes names
stadiums <- read_csv("./data/stadiums-geocoded.csv") %>%
  select(team, capacity, div, latitude, longitude, state) %>%
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

## Connects teamd and stadiums
teams <- stadiums %>%
  left_join(fbs_list, by = c("team" = "School"))

## Cleans Schedule Data
dat_2021 <- clean(read_csv("./data/2021_schedule.csv") %>% rename_all(tolower), stadiums)
dat_2022 <- clean(read_csv("./data/2022_schedule.csv") %>% rename_all(tolower), stadiums)
dat_2023 <- clean(read_csv("./data/2023_schedule.csv") %>% rename_all(tolower), stadiums)
dat_2024 <- clean(read_csv("./data/2024_schedule.csv") %>% rename_all(tolower), stadiums)

## Puts schedules into a list
schedules <- list(
  "2021" = dat_2021,
  "2022" = dat_2022,
  "2023" = dat_2023,
  "2024" = dat_2024
)

## Gets Great Circle lines for 2024
lines <- gc(dat_2024, teams, realinged_teams)

## Calculates distances for each year
teams <- get_distance("2021", teams)
teams <- get_distance("2022", teams)
teams <- get_distance("2023", teams)
teams <- get_distance("2024", teams)

## Adds T/f variable for realinged 
teams <- teams %>%
  mutate(realigned = ifelse(team %in% realinged_teams, TRUE, FALSE))

## Pivots teams to make usable for table
teams_long <- teams %>%
  select(team, total_mileage_2021, total_mileage_2022, total_mileage_2023, total_mileage_2024, realigned) %>%
  pivot_longer(
    cols = starts_with("total_mileage"),
    names_to = "year",
    values_to = "mileage"
  ) %>%
  mutate(year = as.numeric(str_extract(year, "\\d{4}")))

## Creates mileage data
conf_and_realigned_miles <- teams %>%
  filter(!realigned) %>%
  group_by(conference) %>%
  summarise(mileage = round(sum(total_mileage_2024) / n(), digits = 0), bus_mileage = round(sum(bus_mileage_2024) / n(), digits = 0), plane_mileage = round(sum(plane_mileage_2024) / n(), digits = 0))

## creates realinged mileage data
realigned_emissions2 <- teams %>%
  filter(realigned) %>%
  summarise(conference = "Realigned", mileage = round(sum(total_mileage_2024) / nrow(filter(teams, realigned)), digits = 0), bus_mileage = round(sum(bus_mileage_2024) / nrow(filter(teams, realigned)), digits = 0), plane_mileage = round(sum(plane_mileage_2024) / nrow(filter(teams, realigned)), digits = 0))

## binds data and calcs emisions
cr_data <- bind_rows(conf_and_realigned_miles, realigned_emissions2) %>%
  mutate(emissions = (bus_mileage * 3) + (plane_mileage * 19))

## makes data long
cr_data_long <- cr_data %>%
  select(-mileage) %>%
  rename(Bus = bus_mileage, Plane = plane_mileage) %>%
  pivot_longer(
    cols = c(Plane, Bus),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(measurement = "\u200BMileage")

cr_data_long <- bind_rows(cr_data_long, cr_data_long %>% mutate(measurement = "Emissions (10kg)", value = (value * ifelse(type == "Bus", 3, 19)) / 10))

cr_data_long <- cr_data_long %>%
  mutate(conference = factor(conference, levels = unique(cr_data_long$conference[order(cr_data_long$value[cr_data_long$measurement == "Emissions (10kg)"], decreasing = TRUE)]))) %>%
  mutate(type_realinged = paste(type, ifelse(conference == "Realigned", "Realigned", "Not Realigned"), sep = "-"))

table_data <- cr_data %>%
  pivot_longer(cols = c(mileage, bus_mileage, plane_mileage, emissions), names_to = "type", values_to = "value") %>%
  pivot_wider(names_from = conference, values_from = value) %>%
  mutate(type = recode(type, "mileage" = "Total Mileage", "bus_mileage" = "Bus Mileage", "plane_mileage" = "Plane Mileage", "emissions" = "Emissions (kg)"))

table_data <- table_data %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

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

p2 <- ggplot(cr_data_long) +
  geom_bar(aes(x = measurement, y = value, fill = type_realinged),
    position = "stack",
    stat = "identity"
  ) +
  facet_grid(~conference, switch = "x") +
  scale_fill_manual(values = c("Bus-Realigned" = "#ffb5a8", "Plane-Realigned" = "#FF6347", "Bus-Not Realigned" = "grey", "Plane-Not Realigned" = "darkgrey"), name = "Type") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.background = element_rect(fill = NA, color = "white"),
    panel.spacing = unit(0.5, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.background = element_rect(fill = "#F5F5F5", color = NA)
  ) +
  labs(
    x = "Conference/Measurement",
    y = "Mileage/Emissions (10kg)",
    title = "Mileage and Emissions by Conference"
  )

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

p4 <- ggplot(teams_long %>% filter(team != "Hawaii"), aes(x = year, y = mileage, group = team, color = realigned)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text",
    x = 2024, y = max(teams_long$mileage),
    label = "Conference\nRealignment",
    hjust = 1.1, vjust = 1
  ) +
  scale_color_manual(
    values = c(`TRUE` = "#FF6347", `FALSE` = "grey"),
    name = "Realigned", labels = c("No", "Yes")
  ) +
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

p5 <- ggplot() +
  geom_sf(data = US, fill = "lightgray", color = "white", alpha = 0.5) +
  geom_path(data = lines, aes(x = lon, y = lat, group = id, color = realinged), size = 0.3, alpha = 0.7) +
  geom_point(data = stadiums %>% filter(team %in% realinged_teams), aes(x = longitude, y = latitude), size = 0.7, alpha = 0.7) +
  geom_vline(data = time_zones, aes(xintercept = lon), linetype = "dotted", color = "grey") +
  theme_void() +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  scale_color_manual(values = c(`TRUE` = "#FF6347", `FALSE` = "grey"), name = "Realigned", labels = c("No", "Yes")) +
  labs(title = "Travel Lines for FBS Teams") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.box.just = "left",
    panel.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.background = element_rect(fill = "#F5F5F5", color = NA)
  )

realinged_count <- stadiums %>%
  filter(team %in% realinged_teams) %>%
  group_by(state) %>%
  summarise(count = n())

state_data <- map_data("state") %>%
  mutate(region = state.abb[match(region, tolower(state.name))]) %>%
  left_join(realinged_count, by = c("region" = "state"))

p6 <- ggplot(state_data, aes(map_id = region)) +
  geom_map(aes(fill = count), map = state_data) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  scale_fill_continuous(low = "#ffb5a8", high = "#FF6347", na.value = "grey") +
  theme_void() +
  labs(title = "Count of Realigned Schools in Each State", fill = "Count") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.background = element_rect(fill = "#F5F5F5", color = NA)
)
  
## Save plots to files in /out
save_as_image(ft, "./out/table.png", res = 300)
ggsave("./out/total_mileage_fbs.png", plot = p, width = 16, height = 12, units = "in", dpi = 300)
ggsave("./out/total_emissions_by_conference.png", plot = p2, width = 16, height = 12, units = "in", dpi = 300)
ggsave("./out/total_mileage_map.png", plot = p3, width = 16, height = 12, units = "in", dpi = 300)
ggsave("./out/mileage_over_years_line.png", plot = p4, width = 16, height = 12, units = "in", dpi = 300)
ggsave("./out/school_travel_lines.png", plot = p5, width = 16, height = 12, units = "in", dpi = 300)
ggsave("./out/realigned_schools_count_map.png", plot = p6, width = 16, height = 12, units = "in", dpi = 300)
