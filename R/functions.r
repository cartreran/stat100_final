library(dplyr)
library(geosphere)

clean <- function(dat, stadiums) {
    dat %>%
        mutate(
            across(everything(), ~ gsub("\u00A0", " ", .))
        ) %>%
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
}
#nolint start
get_distance <- function(year, teams) {
    for (i in 1:nrow(teams)) {
        total_mileage <- 0
        bus_mileage <- 0
        plane_mileage <- 0
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
                if(distance < 351 && team != "Hawaii") {
                    bus_mileage <- bus_mileage + distance
                } else {
                    plane_mileage <- plane_mileage + distance
                }
                total_mileage <- total_mileage + distance
            }
        }
        total_mileage <- round(total_mileage)
        bus_mileage <- round(bus_mileage)
        plane_mileage <- round(plane_mileage)
        col_name <- paste0("total_mileage_", year)
        bus_col_name <- paste0("bus_mileage_", year)
        plane_col_name <- paste0("plane_mileage_", year)
        teams[[col_name]][i] <- 0
        teams[[bus_col_name]][i] <- 0
        teams[[plane_col_name]][i] <- 0
        teams[[bus_col_name]][i] <- bus_mileage
        teams[[plane_col_name]][i] <- plane_mileage
        teams[[col_name]][i] <- total_mileage
    }
    return(teams)
}

gc <- function(data, stadiums, realinged) {
  gc_lines <- list()
  for (i in seq_len(nrow(data))) {
    home <- data$home[i]
    away <- data$away[i]
    home_cord <- c(as.numeric(stadiums[stadiums$team == home, "longitude"]), as.numeric(stadiums[stadiums$team == home, "latitude"]))
    away_cord <- c(as.numeric(stadiums[stadiums$team == away, "longitude"]), as.numeric(stadiums[stadiums$team == away, "latitude"]))
    gc_line <- gcIntermediate(
      home_cord,
      away_cord,
      n = 100,
      addStartEnd = TRUE,
      breakAtDateLine = FALSE
    )
    gc_lines[[i]] <- data.frame(
      lon = gc_line[, 1],
      lat = gc_line[, 2],
      id = i,
      team = away,
      realinged = ifelse(away %in% realinged, TRUE, FALSE)
    )
  }
  return(do.call(rbind, gc_lines))
}
