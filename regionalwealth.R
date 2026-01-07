library(tidyverse)
library(tidycensus)
library(fredr)
setwd(Sys.getenv("THESIS_WD"))
source("functions.R")

# built-in state codes + DC
state_codes = c(state.abb, "DC")

# import state FIP codes + DC
data(fips_codes)
state_fips_crosswalk = fips_codes |>
  select(state, state_code) |>
  transmute(STATEFIP = as.integer(state_code), state) |>
  distinct(state, STATEFIP, .keep_all = TRUE)

# series names
population = "POP"
ngdp = "NGSP"

# state populations
pop_long = map_dfr(state_codes, \(st) {
  sid = paste0(st, population)
  fetch_fred_series(sid) |>
    mutate(state = st, series = population)
})

# state gdps (nominal, since ATUS incomes are nominal)
gdp_long = map_dfr(state_codes, \(st) {
  sid = paste0(st, ngdp)
  fetch_fred_series(sid) |>
    mutate(state = st, series = ngdp)
})

# get nominal GDP per capita
state_gdppc = bind_rows(pop_long, gdp_long) |>
  mutate(YEAR = as.integer(format(date, "%Y"))) |>
  select(state, YEAR, series, value) |>
  pivot_wider(names_from = series, values_from = value) |>
  # nominal gdp in millions of dollars
  # population in thousands of persons
  # (NGSP * 1e6) / (POP * 1e3) = (NGSP / POP) * 1000 dollars per person
  mutate(gdp_pc_nominal = (NGSP / POP) * 1000) |>
  select(state, YEAR, POP, NGSP, gdp_pc_nominal) |>
  # find FIP codes
  inner_join(state_fips_crosswalk, by=c("state"))

# save
write.csv(state_gdppc,"regionalwealth.csv", 
          row.names = FALSE)
