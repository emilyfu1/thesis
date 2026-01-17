library(tidyverse)
library(haven)
library(tidyr)
library(stringr)

# actlines and directories
source("UKTUS_params.R")

# regional wealth
source("regionalwealth.R")

# setwd
setwd(wd)

################################################################################
############################## ACTIVITY-LEVEL DATA #############################
################################################################################

# create activity list for main activities
num_slots = 144
act1_names = c()
act2_names = c()
wher_names = c()
wit0_names = c()
wit1_names = c()
wit2_names = c()
wit3_names = c()
wit4_names = c()
wit5_names = c()
wit6_names = c()

for (i in 1:num_slots) {
  if (i < 10) {
    add_string = paste0("00", as.character(i))
  } else if (i >= 10 & i < 100) {
    add_string = paste0("0", as.character(i))
    } else add_string = as.character(i)
  act1_names = c(act1_names, paste0("act1_", add_string))
  act2_names = c(act2_names, paste0("act2_", add_string))
  wher_names = c(wher_names, paste0("wher_", add_string))
  wit0_names = c(wit0_names, paste0("wit0_", add_string))
  wit1_names = c(wit1_names, paste0("wit1_", add_string))
  wit2_names = c(wit2_names, paste0("wit2_", add_string))
  wit3_names = c(wit3_names, paste0("wit3_", add_string))
  wit4_names = c(wit4_names, paste0("wit4_", add_string))
  wit5_names = c(wit5_names, paste0("wit5_", add_string))
  wit6_names = c(wit6_names, paste0("wit6_", add_string))
}

# import data
data_activities_2000_wide = read_dta(paste0(uktus_2000_direct, 
                                            "diary_data_8.dta"))

id_vars = c("sn1", "sn2","sn3","sn4","dyear","dmonth","dday","ddayofwk","dtype")
slot_regex = "^(act1|act2|wher|wit\\d+)_\\d{3}$"

timeuse_vars = c(act1_names, act2_names, wher_names, wit0_names,
                 wit1_names, wit2_names, wit3_names, wit4_names,
                 wit5_names, wit6_names)

columns_to_keep = c(id_vars, timeuse_vars)

data_activities_2000_long = data_activities_2000_wide |>
  zap_labels() |>
  select(all_of(columns_to_keep)) |>
  pivot_longer(
    cols = matches(slot_regex),
    names_to = c("var", "slot"),
    names_pattern = "^(.*)_(\\d{3})$",
    values_to = "value") |>
  select(all_of(id_vars), slot, var, value) |>
  pivot_wider(names_from = var,
              values_from = value) |>
  # each interval is 10 minutes long
  mutate(eptime = 10) |>
  rename(serial = sn2, pnum = sn3, DiaryDay_Act = sn4, IMonth = dmonth, 
         IYear = dyear, whatdoing = act1, What_Oth1 = act2)

# only keep information collected at the same point as the time use data
diarymonth_households_2000 = unique_interview_months(data_activities_2000_long)

# find time diaries and number of diaries everyone completes
individual_diaries_2000 = unique_interview_diaries(data_activities_2000_long)
