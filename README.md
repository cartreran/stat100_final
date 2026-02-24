# STAT 100 Final Project — Environmental Impact of FBS Conference Realignment

This repository contains our STAT 100 final project analyzing how recent **FBS college football conference realignment** (e.g., SEC/Big Ten/ACC/Pac-12 changes) affects the **environmental impact of travel**, primarily by increasing travel distances for teams.

## Project idea (high-level)
Realignment has shifted college football from a mostly regional structure toward more national schedules. Our project investigates how these changes influence:
- team travel distances (mileage),
- differences across years (pre/post realignment),
- and downstream implications like travel-related emissions.

The main write-up is an R Markdown paper titled **“Environmental Impact of FBS Conference Realignment.”**

## What’s in this repo
- **Final Paper/**: the primary paper source (`Final Paper.Rmd`)
- **essay_FINAL.rmd**: an additional paper draft/format of the write-up
- **Data Analysis.rmd**: analysis work and/or slides (ioslides)
- **FInal Presentation/**: the final presentation Rmd + supporting files
- **Project Proposal.Rmd**: the early proposal and topic exploration
- **R/**: helper functions used by the analysis (e.g., distance calculations, cleaning)
- **data/**: datasets used in the analysis (schedules, team lists, etc.)
- **images/**: images used in the proposal/paper/presentation

## How to run (typical)
1. Open the project in RStudio.
2. Install required packages (tidyverse, readr, dplyr, ggplot2, geosphere, etc.).
3. Knit the main report:
   - `Final Paper/Final Paper.Rmd`

## Authors
Carter Nabors
