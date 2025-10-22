library(readxl)
require(tidyverse)
devtools::document()

## Load input data from UI
path_to_UI <- "Templates/Full carbon calculator for windfarms on peatlands - Version 2.14.1.xlsx" # select user input spreadsheet

dat <- getData(path)

core.dat <- dat$core.dat
forestry.dat <- dat$forestry.dat
construct.dat <- dat$construct.dat
rm(dat)

## Calculate emissions savings - update parameter interaction
# Emissions_saving <- Windfarm_CO2_emissions_saving()
