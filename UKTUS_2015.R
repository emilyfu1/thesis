library(tidyverse)
library(haven)

wd = Sys.getenv("THESIS_WD")
setwd(wd)

# actlines 
source("UKTUS_actlines.R")
data_2015_direct = "/UKDA-8128-stata/stata/stata11_se/"

data_activities = read_dta(paste0(wd, data_2015_direct, "uktus15_diary_ep_long.dta"))
data_hh = read_dta(paste0(wd, data_2015_direct, "uktus15_household.dta"))
data_individual = read_dta(paste0(wd, data_2015_direct, "uktus15_individual.dta"))

# household budget: how much the household could earn if all members worked all 
# the time, in addition to any unearned income
# so, I keep people who completed both days
data_activities = data_activities |>
  filter()