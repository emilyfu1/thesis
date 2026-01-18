library(tidyverse)
library(readxl)
library(stringr)

# actlines and directories
source("UKTUS_params.R")

# setwd
setwd(wd)

################################################################################
################################## GDP DEFLATOR ################################
################################################################################

gdp_deflator = read_excel(paste0(data_direct, "CGDP_Deflators_Qtrly_National_Accounts_December_2025_update.xlsx"), 
                          skip = 6)[c("Calendar year", "2024-25 = 100")] 
names(gdp_deflator) = c("year", "deflator_2024values")
gdp_deflator = gdp_deflator |>
  filter(year %in% c(2000, 2014)) |>
  mutate(deflator_factor = 100 / as.numeric(deflator_2024values))

# each deflator value
deflator_2000 = gdp_deflator$deflator_factor[which(gdp_deflator$year == 2000)]
deflator_2014 = gdp_deflator$deflator_factor[which(gdp_deflator$year == 2014)]

################################################################################
############################### UK GDP PER CAPITA ##############################
################################################################################

################################ UK nominal GDP ################################

# nominal GDP
regional_gdp = read_excel(paste0(data_direct, "regionalwealth_all.xlsx"), 
                          sheet = "Table 5",
                          skip = 1)[c("Region name", "2000", "2014")] 
names(regional_gdp) = c("dgorpaf_name", "ngdp_2000", "ngdp_2014")


# nominal GDP in each year

regional_gdp_2014 = regional_gdp[c("dgorpaf_name", "ngdp_2014")] |> 
  select(dgorpaf_name, ngdp_2014) |>
  # make names consistent
  mutate(dgorpaf_name = toupper(trimws(dgorpaf_name)),
         ngdp_2014 = as.numeric(ngdp_2014)) |>
  filter(dgorpaf_name %in% region_names)  |>
  # get rid of smaller regions that share a name
  group_by(dgorpaf_name) |>
  slice_max(ngdp_2014, n = 1, with_ties = FALSE) |>
  ungroup() |>
  # get corresponding value
  mutate(dgorpaf = unname(region_map[dgorpaf_name]))

regional_gdp_2000 = regional_gdp[c("dgorpaf_name", "ngdp_2000")] |> 
  select(dgorpaf_name, ngdp_2000) |>
  # make names consistent
  mutate(dgorpaf_name = toupper(trimws(dgorpaf_name)),
         ngdp_2000 = as.numeric(ngdp_2000)) |>
  filter(dgorpaf_name %in% region_names) |>
  # get rid of smaller regions that share a name
  group_by(dgorpaf_name) |>
  slice_max(ngdp_2000, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(dgorpaf = unname(region_map[dgorpaf_name]))

################################ UK population #################################

regional_pop_2014 = read_excel(paste0(data_direct, "population_2011_2024.xlsx"), 
                          sheet = "MYE4",
                          skip = 7)[c("Name", "Mid-2014")]
names(regional_pop_2014) = c("dgorpaf_name", "pop_2014")
regional_pop_2014 = regional_pop_2014 |>
  # make names consistent
  mutate(dgorpaf_name = toupper(trimws(dgorpaf_name)),
         pop_2014 = as.numeric(pop_2014)) |>
  filter(dgorpaf_name %in% region_names) |>
  # get rid of smaller regions that share a name
  group_by(dgorpaf_name) |>
  slice_max(pop_2014, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(dgorpaf = unname(region_map[dgorpaf_name]))

regional_pop_2000 = read_excel(paste0(data_direct, "population_2000.xls"), 
                               sheet = "Mid-2000 Persons")[c("Name", "ALL AGES")]
names(regional_pop_2000) = c("dgorpaf_name", "pop_2000")
regional_pop_2000 = regional_pop_2000 |>
  # make names consistent
  mutate(dgorpaf_name = toupper(trimws(dgorpaf_name)),
         pop_2000 = as.numeric(pop_2000)) |>
  filter(dgorpaf_name %in% region_names) |>
  # get rid of smaller regions that share a name
  group_by(dgorpaf_name) |>
  slice_max(pop_2000, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(dgorpaf = unname(region_map[dgorpaf_name]))

########################## get nominal gdps per capita #########################

regionalwealth_2014 = regional_gdp_2014 |>
  select(dgorpaf, ngdp_2014) |>
  inner_join(regional_pop_2014, by = c("dgorpaf")) |>
  # nominal gdp is in millions
  mutate(ngdppc_2014 = ngdp_2014 * 1e6 / pop_2014) |>
  # deflate
  mutate(rgdppc_2014 = ngdppc_2014 * deflator_2014) |>
  select(dgorpaf, rgdppc_2014)
  
regionalwealth_2000 = regional_gdp_2000 |>
  select(dgorpaf, ngdp_2000) |>
  inner_join(regional_pop_2000, by = c("dgorpaf")) |>
  # nominal gdp is in millions
  mutate(ngdppc_2000 = ngdp_2000 * 1e6 / pop_2000) |>
  # deflate
  mutate(rgdppc_2000 = ngdppc_2000 * deflator_2000) |>
  select(dgorpaf, rgdppc_2000)