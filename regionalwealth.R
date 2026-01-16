library(tidyverse)
library(tidycensus)
library(fredr)
library(readxl)
library(stringr)

# actlines and directories
source("UKTUS_params.R")

# setwd
setwd(wd)

################################################################################
############################### UK GDP PER CAPITA ##############################
################################################################################

################################ UK nominal GDP ################################

# nominal GDP
regional_gdp = read_excel(paste0(data_direct, "regionalwealth_all.xlsx"), 
                          sheet = "Table 5",
                          skip = 1)[c("Region name", "2000", "2014")] 
names(regional_gdp) = c("dgorpaf", "ngdp_2000", "ngdp_2014")


# nominal GDP in each year

regional_gdp_2014 = regional_gdp[c("dgorpaf", "ngdp_2014")] |> 
  select(dgorpaf, ngdp_2014) |>
  # make names consistent
  mutate(dgorpaf = toupper(trimws(dgorpaf)),
         ngdp_2014 = as.numeric(ngdp_2014)) |>
  filter(dgorpaf %in% region_names)  |>
  # get rid of smaller regions that share a name
  group_by(dgorpaf) |>
  slice_max(ngdp_2014, n = 1, with_ties = FALSE) |>
  ungroup()

regional_gdp_2000 = regional_gdp[c("dgorpaf", "ngdp_2000")] |> 
  select(dgorpaf, ngdp_2000) |>
  # make names consistent
  mutate(dgorpaf = toupper(trimws(dgorpaf)),
         ngdp_2000 = as.numeric(ngdp_2000)) |>
  filter(dgorpaf %in% region_names) |>
  # get rid of smaller regions that share a name
  group_by(dgorpaf) |>
  slice_max(ngdp_2000, n = 1, with_ties = FALSE) |>
  ungroup()

################################ UK population #################################

regional_pop_2014 = read_excel(paste0(data_direct, "population_2011_2024.xlsx"), 
                          sheet = "MYE4",
                          skip = 7)[c("Name", "Mid-2014")]
names(regional_pop_2014) = c("dgorpaf", "pop_2014")
regional_pop_2014 = regional_pop_2014 |>
  # make names consistent
  mutate(dgorpaf = toupper(trimws(dgorpaf)),
         pop_2014 = as.numeric(pop_2014)) |>
  filter(dgorpaf %in% region_names) |>
  # get rid of smaller regions that share a name
  group_by(dgorpaf) |>
  slice_max(pop_2014, n = 1, with_ties = FALSE) |>
  ungroup()

regional_pop_2000 = read_excel(paste0(data_direct, "population_2000.xls"), 
                               sheet = "Mid-2000 Persons")[c("Name", "ALL AGES")]
names(regional_pop_2000) = c("dgorpaf", "pop_2000")
regional_pop_2000 = regional_pop_2000 |>
  # make names consistent
  mutate(dgorpaf = toupper(trimws(dgorpaf)),
         dgorpaf = if_else(dgorpaf == "EASTERN", "EAST", dgorpaf),
         pop_2000 = as.numeric(pop_2000)) |>
  filter(dgorpaf %in% region_names) |>
  # get rid of smaller regions that share a name
  group_by(dgorpaf) |>
  slice_max(pop_2000, n = 1, with_ties = FALSE) |>
  ungroup()

########################## get nominal gdps per capita #########################

regionalwealth_2014 = regional_gdp_2014 |>
  inner_join(regional_pop_2014, by = c("dgorpaf")) |>
  # nominal gdp is in millions
  mutate(ngdppc_2014 = ngdp_2014 * 1e6 / pop_2014)
  

regionalwealth_2000 = regional_gdp_2000 |>
  inner_join(regional_pop_2000, by = c("dgorpaf")) |>
  # nominal gdp is in millions
  mutate(ngdppc_2000 = ngdp_2000 * 1e6 / pop_2000)

################################################################################
########################## ARCHIVE: US GDP PER CAPITA ##########################
################################################################################

atus_direct = paste0(wd, "/atus_data/")

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
write.csv(state_gdppc,paste0(atus_direct, "regionalwealth_US.csv"), 
          row.names = FALSE)
