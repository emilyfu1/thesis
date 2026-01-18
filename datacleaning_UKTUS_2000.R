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

# create vectors with column names
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

# household/individual identifiers
id_vars = c("sn1", "sn2","sn3","sn4","dyear","dmonth","dday","ddayofwk","dtype")
slot_regex = "^(act1|act2|wher|wit\\d+)_\\d{3}$"

# columns to use
timeuse_vars = c(act1_names, act2_names, wher_names, wit0_names,
                 wit1_names, wit2_names, wit3_names, wit4_names,
                 wit5_names, wit6_names)
columns_to_keep = c(id_vars, timeuse_vars)

# convert to long format
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
  # make household identifier equivalent to 2015 data
  mutate(serial = as.integer(paste0(sn2, "0", sn1))) |>
  rename(pnum = sn3, DiaryDay_Act = sn4, daynum = dday,
         IMonth = dmonth, IYear = dyear, whatdoing = act1, What_Oth1 = act2)

# only keep information collected at the same point as the time use data
diarymonth_households_2000 = unique_interview_months(data_activities_2000_long)

# find time diaries and number of diaries everyone completes
individual_diaries_2000 = unique_interview_diaries(data_activities_2000_long)

# finding time use
activity_summaries_2000 = data_activities_2000_long |>
  select(serial, pnum, daynum, IMonth, IYear, eptime, whatdoing, What_Oth1,
         wit0, wit1, wit2, wit3, wit4, wit5, wit6) |>
  
  # secondary activities and stuff
  mutate(
    activity1_is_leisure = whatdoing %in% leisure_actlines,
    activity1_is_leisure_r = whatdoing %in% restrict_actlines,
    activity1_is_childcare = whatdoing %in% childcare_actlines,
    activity2_is_leisure = What_Oth1 %in% leisure_actlines,
    activity2_is_leisure_r = What_Oth1 %in% restrict_actlines,
    activity2_is_childcare = What_Oth1 %in% childcare_actlines,
    
    # private (no relevant household members present)
    # activities where "who" isn't asked are considered private
    activity_private = wit1 == 0 & wit2 == 0 & wit3 == 0,
    # spouse not present (as equivalent as possible)
    activity_excludesspouse = wit3 == 0,
    
    # general: is leisure?
    activity_is_leisure = (activity1_is_leisure | activity2_is_leisure),
    activity_is_leisure_r = (activity1_is_leisure_r | activity2_is_leisure_r),
    
    # general: is private leisure?
    private_leisure = activity_is_leisure & activity_private,
    private_leisure_r = activity_is_leisure_r & activity_private,
    
    # general: is childcare?
    activity_ischildcare = (activity1_is_childcare | activity2_is_childcare | 
                              wit1 == 1 | wit2 == 1),
    
    # is no-spouse childcare?
    childcare_nospouse = activity_ischildcare & activity_excludesspouse) |>
  
  # add up time use in hours
  group_by(serial, pnum) |>
  summarise(
    total_leisure = sum(eptime[activity_is_leisure], na.rm = TRUE) / 60,
    total_private_leisure = sum(eptime[private_leisure], na.rm = TRUE) / 60,
    total_leisure_r = sum(eptime[activity_is_leisure_r], na.rm = TRUE) / 60,
    total_private_leisure_r = sum(eptime[private_leisure_r], na.rm = TRUE) / 60,
    
    total_childcare = sum(eptime[activity_ischildcare], na.rm = TRUE)  / 60,
    total_childcare_nospouse = sum(eptime[childcare_nospouse], na.rm = TRUE) / 60,
    .groups = "drop")

################################################################################
############################# HOUSEHOLD-LEVEL DATA #############################
################################################################################

# household data
data_hh_2000 = read_dta(paste0(uktus_2000_direct, 
                                      "hhld_data_6.dta")) |>
  # make household identifier equivalent to 2015 data
  mutate(serial = as.integer(paste0(sn2, "0", sn1)))

# relationships in individual data
all_relationships_2000 = data_hh_2000 |>
  # keep only household id + relationship matrix
  select(serial, matches("^rel\\d{2}_\\d{2}$")) |>
  pivot_longer(cols = matches("^rel\\d{2}_\\d{2}$"),
               names_to = "rel_var",
               values_to = "relation_to_pnum") |>
  # parse rel02_06 -> from=2, to=6
  mutate(pnum = as.integer(str_sub(rel_var, 4, 5)),
         relevant_person = as.integer(str_sub(rel_var, 7, 8))) |>
  select(serial, pnum, relevant_person, relation_to_pnum) |>
  filter(!is.na(relation_to_pnum)) |>
  
  # dummy type of relationship
  mutate(pnum_is_spouse = relation_to_pnum < 3,
         pnum_is_child = relation_to_pnum >=3 & relation_to_pnum <= 5,
         pnum_is_parent = relation_to_pnum >= 7 & relation_to_pnum <= 9)

################################################################################
############################ INDIVIDUAL-LEVEL DATA #############################
################################################################################

data_individual_2000 = read_dta(paste0(uktus_2000_direct, 
                                       "individual_data_5.dta")) |>
  zap_labels() |>
  # make household identifier equivalent to 2015 data
  mutate(serial = as.integer(paste0(sn2, "0", sn1))) |>
  
  rename(pnum = sn3, IMonth = hmonth, IYear = hyear, DMSex = isex,
         DVAge = iage) |>
  
  # only keep diary month
  # inner_join(diarymonth_households_2015, by = c("serial", "pnum", "IMonth")) |>
  
  # keep if observation has age, sex
  filter(DMSex != 3, DVAge >= 0) |>
  mutate(male = DMSex == 1, # sex dummy
         is_resp = pnum == 1, # person 1 is always respondent
         
         # Highest qualification obtained, simplified into three categories. 
         # Category 2: is equivalent to an undergraduate degree or higher. 
         # Category 1: end-of-school diplomas e.g. A levels, IBDP
         # Category 0: is anything less than that e.g. GSCEs
         educ = case_when(
           # Category 2: degree or higher
           hiqual4 %in% c(1, 2) ~ 2,
           
           # Category 1: end-of-school / pre-university
           hiqual4 %in% c(3, 7) ~ 1,
           
           # Category 0: below end-of-school
           hiqual4 %in% c(4, 5, 6, 8, 9, 10, 11, 12) ~ 0,
           
           # Missing / invalid
           TRUE ~ NA_real_))

################################### Children ###################################

# find all children in data with sex and age (includes adult kids)
data_kids_2000 = find_kids(relationships_data = all_relationships_2000, 
                           individual_data = data_individual_2000)

kids_counts_2000 = count_kids(data_kids_2000)

kids_age_dist_2000 = find_kid_ages(data_kids_2000)

kids_age_wide_2000 = find_kid_ages_wide(data_kids_2000)

############################## Parents and couples #############################

################################################################################
##################### CALCULATING HOUSEHOLD CHARACTERISTICS ####################
################################################################################
