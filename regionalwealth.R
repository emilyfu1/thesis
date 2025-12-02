library(tidyverse)
library(stringr)
setwd("/Users/emilyfu/Desktop/school/thesis")

# import data

# annual population by state
pop_annual_level = read.csv("pop_annual_level.csv")

# nominal gdp by state 
gdp_n_state = read.csv("gdp_n_state.csv", skip = 3)
# (skip headers and footnotes)
gdp_n_state = gdp_n_state[1:(dim(gdp_n_state)[1]-3),]
  
# convert state fips code

gdp_n_state = gdp_n_state |>
  mutate(STATEFIP = as.integer(str_remove(GeoFips, "000$")))

