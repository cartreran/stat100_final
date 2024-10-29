library(tidyverse)
library(readr)

transfer <- read.csv("./data/Recruitment/transfer.csv")
#TEST
ggplot(data = transfer, aes(x = `YEAR`, y = `TRATE`)) +
  geom_line() +
  geom_point()