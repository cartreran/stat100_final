library(tidyverse)
library(readr)

transfer <- read.csv("./data/transfer.csv")

transfer$YEAR <- as.factor(transfer$YEAR)

## transfers by year
ggplot() +
    geom_col(data = transfer, aes(x = `YEAR`, y = `TEAM_COUNT`), show.legend = TRUE, just = 1) +
    theme_minimal() +
    labs(x = "Year", y = "Transfer Count", title = "Conference Transfers by Year")

