library(dplyr)

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
