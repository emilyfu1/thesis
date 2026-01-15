library(tidyverse)
library(haven)

# setwd
wd = Sys.getenv("THESIS_WD")
setwd(wd)

# actlines and directories
source("UKTUS_params.R")

################################################################################
############################## ACTIVITY-LEVEL DATA #############################
################################################################################

# activity level data
data_activities = read_dta(paste0(uktus_2015_direct, "uktus15_diary_ep_long.dta"))

# find which month to keep in the individual and household data
# only keep information collected at the same point as the time use data
diarymonth_households = data_activities |>
  distinct(serial, pnum, IMonth) |>
  arrange(serial, pnum)

# find time diaries and number of diaries everyone completes
individual_diaries = data_activities |>
  distinct(serial, pnum, DiaryDay_Act) |>
  group_by(serial, pnum) |>
  # household budget: i need to indicate how many days everyone has completed
  # so that i calculate expenditure and budget based on number of days
  mutate(num_diaries_filled = n()) |>
  distinct(serial, pnum, num_diaries_filled)

# finding time use
activity_summaries = data_activities |>
  select(serial, pnum, daynum, IMonth, IYear, eptime, whatdoing, What_Oth1,
         What_Oth2, What_Oth3, WithAlone, WithSpouse, WithChild, WithOther) |>
  
  # secondary activities and stuff
  mutate(
    activity1_is_leisure = whatdoing %in% leisure_actlines,
    activity1_is_leisure_r = whatdoing %in% restrict_actlines,
    activity1_is_childcare = whatdoing %in% childcare_actlines,
    activity2_is_leisure = What_Oth2 %in% leisure_actlines,
    activity2_is_leisure_r = What_Oth2 %in% restrict_actlines,
    activity2_is_childcare = What_Oth2 %in% childcare_actlines,
    activity3_is_leisure = What_Oth3 %in% leisure_actlines,
    activity3_is_leisure_r = What_Oth3 %in% restrict_actlines,
    activity3_is_childcare = What_Oth3 %in% childcare_actlines,
    
    # private (no relevant household members present)
    # activities where "who" isn't asked are considered private
    activity_private = WithSpouse == 0 & WithChild == 0,
    # spouse not present
    activity_excludesspouse = WithSpouse == 0,
    
    # general: is leisure?
    activity_is_leisure = (activity1_is_leisure | activity2_is_leisure | 
                             activity3_is_leisure),
    activity_is_leisure_r = (activity1_is_leisure_r | activity2_is_leisure_r | 
                               activity3_is_leisure_r),
    
    # general: is private leisure?
    private_leisure = activity_is_leisure & activity_private,
    private_leisure_r = activity_is_leisure_r & activity_private,
    
    # general: is childcare?
    activity_ischildcare = (activity1_is_childcare | activity2_is_childcare | 
                              activity3_is_childcare),
    
    # is no-spouse childcare?
    childcare_nospouse = activity_ischildcare & activity_excludesspouse) |>
  
  # add up time use in hours
  group_by(serial, pnum) |>
  summarise(
    total_leisure = sum(eptime[activity_is_leisure], na.rm = TRUE) / 60,
    total_private_leisure = sum(eptime[private_leisure], na.rm = TRUE) / 60,
    total_leisure_r = sum(eptime[activity1_is_leisure_r], na.rm = TRUE) / 60,
    total_private_leisure_r = sum(eptime[private_leisure_r], na.rm = TRUE) / 60,
    
    total_childcare = sum(eptime[activity_ischildcare], na.rm = TRUE)  / 60,
    total_childcare_nospouse = sum(eptime[childcare_nospouse], na.rm = TRUE) / 60,
    .groups = "drop")

################################################################################
############################# HOUSEHOLD-LEVEL DATA #############################
################################################################################

# household data
data_hh = read_dta(paste0(uktus_2015_direct, "uktus15_household.dta")) |>
  inner_join(diarymonth_households, by = c("serial", "IMonth"))

################################################################################
############################ INDIVIDUAL-LEVEL DATA #############################
################################################################################

# individual level data
data_individual = read_dta(paste0(uktus_2015_direct, "uktus15_individual.dta")) |>
  # keep if observation has age, education, sex
  filter(DVAge >= 0, DMSex >= 0) |>
  mutate(male = DMSex == 1, # sex dummy
         is_resp = pnum == SelPer, # indicate respondent
         
         # Highest qualification obtained, simplified into three categories. 
         # Category 2: is equivalent to an undergraduate degree or higher. 
         # Category 1: end-of-school diplomas e.g. A levels, IBDP
         # Category 0: is anything less than that e.g. GSCEs
         educ_cat = case_when(
           # Category 2: degree or higher
           HiQual %in% c(1, 2, 7, 8) ~ 2,
           
           # Category 1: end-of-school / pre-university
           HiQual %in% c(3, 4, 5, 6, 9, 10, 11, 12, 13, 
                         16, 17, 18, 19, 24, 32) ~ 1,
           
           # Category 0: below end-of-school
           HiQual %in% c(14, 15, 20, 21, 22, 23, 25, 26, 
                         27, 28, 29, 30, 31, 34) ~ 0,
           
           # Missing / invalid
           TRUE ~ NA_real_))

# relationships in individual data
all_relationships = data_individual |> 
  pivot_longer(cols = starts_with("Relate"), 
               names_to = "relate_var", 
               values_to = "relation_to_pnum") |> 
  mutate(relevant_person = as.integer(gsub("Relate", "", relate_var))) |> 
  select(serial, pnum, relevant_person, relation_to_pnum) |>
  # filter down to each household size (filter out not applicables)
  filter(relation_to_pnum > 0) |>
  
  # dummy type of relationship
  mutate(pnum_is_spouse = relation_to_pnum <= 3,
         pnum_is_child = relation_to_pnum >=4 & relation_to_pnum <= 6,
         pnum_is_parent = relation_to_pnum >= 8 & relation_to_pnum <= 10)

################################### Children ###################################

# find all children in data with sex and age (includes adult kids)
data_kids = all_relationships |>
  filter(pnum_is_child) |>
  # get identifiers
  distinct(serial, pnum) |>
  # get individual characteristics
  inner_join(data_individual, by = c("serial", "pnum")) |>
  select(serial, pnum, DVAge, DMSex)

# how many kids of each sex
kids_counts = data_kids |>
  group_by(serial) |>
  summarise(num_kids_total = n(),
            num_kids_male = sum(DMSex == 1, na.rm = TRUE),
            num_kids_female = sum(DMSex == 2, na.rm = TRUE),
            .groups = "drop")

# kid age distribution
kids_age_dist = data_kids |>
  group_by(serial) |>
  summarise(kid_age_min  = min(DVAge, na.rm = TRUE),
            kid_age_max  = max(DVAge, na.rm = TRUE),
            kid_age_mean = mean(DVAge, na.rm = TRUE),
            
            n_kid_aged_0_2 = sum(DVAge <= 2, na.rm = TRUE),
            n_kid_aged_3_5 = sum(DVAge >= 3  & DVAge <= 5, na.rm = TRUE),
            n_kid_aged_6_10 = sum(DVAge >= 6  & DVAge <= 10, na.rm = TRUE),
            n_kid_aged_11_13  = sum(DVAge >= 11 & DVAge <= 13, na.rm = TRUE),
            n_kid_aged_14_17 = sum(DVAge >= 14 & DVAge <= 17, na.rm = TRUE),
            .groups = "drop") |>
  select(serial, kid_age_min, kid_age_max, kid_age_mean, n_kid_aged_0_2,
         n_kid_aged_3_5, n_kid_aged_6_10, n_kid_aged_11_13, n_kid_aged_14_17)

# get ages of each kid
kids_age_wide = data_kids |>
  arrange(serial, desc(DVAge)) |>
  group_by(serial) |>
  mutate(kid_index = row_number()) |>
  ungroup() |>
  select(serial, kid_index, DVAge) |>
  pivot_wider(names_from = kid_index,
              values_from = DVAge,
              names_prefix = "age_of_kid_")

############################## Parents and couples #############################

# find spouse/partner pairs in data
spouse_pairs = all_relationships |> 
  filter(pnum_is_spouse) |> 
  distinct(serial, pnum, relevant_person) |> 
  # keep only pairs of spouses/partners
  group_by(serial) |>
  filter(n() == 2) |>
  ungroup() |>
  # rename as spouse_pnum 
  rename(spouse_pnum = relevant_person) |> 
  arrange(serial, pnum)

# parents
data_working_parents = data_individual |>
  # keep only hetero couples with child in household
  filter(NumSSex == 0, NumChild > 0) |>
  
  # show number of diaries and only keep diary month
  inner_join(individual_diaries, by = c("serial", "pnum")) |>
  inner_join(diarymonth_households, by = c("serial", "pnum", "IMonth")) |>
  # merge with time use
  inner_join(activity_summaries, by = c("serial", "pnum")) |>
  
  # merge kid information 
  inner_join(kids_counts, by = c("serial")) |>
  inner_join(kids_age_dist, by = c("serial")) |>
  inner_join(kids_age_wide, by = c("serial")) |>
  
  # identify couples
  inner_join(spouse_pairs, by = c("serial", "pnum")) |>

  group_by(serial) |>
  
  # i'll keep people who are usually working since we have december data
  filter(all(NetWkly > 0), all(HrWkUS > 0)) |>
  
  # check for couples who both have time diaries (filter after both joins)
  mutate(spouse_present = spouse_pnum %in% pnum) |>

  ungroup() |> 
  
  # only keep couples who both have time diaries (filter after both joins)
  filter(spouse_present) |>
  
  # indicate whether someone is the spouse
  mutate(is_spouse = !is_resp) |>
  
  # individual expenditure calculated using time use
  mutate(hrly_wage = NetWkly / HrWkUS, # calculated hourly wages
         
         # leisure and childcare expenditure
         total_leisure_exp = hrly_wage * total_leisure,
         total_leisure_exp_r = hrly_wage * total_leisure_r,
         private_leisure_exp = hrly_wage * total_private_leisure,
         private_leisure_exp_r = hrly_wage * total_private_leisure_r,
         total_childcare_exp = hrly_wage * total_childcare,
         nospouse_childcare_exp = hrly_wage * total_childcare_nospouse,
         # individual contribution to household budget
         y_individual = hrly_wage * 24 * num_diaries_filled) |>
  
  # keep households with under-18 kids
  filter(kid_age_min < 18)
  
################################################################################
##################### CALCULATING HOUSEHOLD CHARACTERISTICS ####################
################################################################################

# create gender-specific versions of variables
vars_to_suffix = c(
  "hrly_wage", "educ_cat", "HrWkUS", "NetWkly", "DVAge",
  "total_leisure", "total_leisure_r", "total_private_leisure",
  "total_private_leisure_r", "total_childcare", "total_childcare_nospouse",
  "total_leisure_exp", "total_leisure_exp_r", "private_leisure_exp",
  "private_leisure_exp_r", "total_childcare_exp", "nospouse_childcare_exp",
  "y_individual", "pnum", "spouse_pnum")

# we should get 634 individuals and 634 / 2 households
sharing_est_data = data_working_parents |>
  # letter for creating variable names
  mutate(sex_tag = if_else(male, "m", "f")) |>
  select(
    serial, sex_tag, dgorpaf, all_of(vars_to_suffix),
    # child info (household-level already, duplicated across spouses)
    num_kids_total, num_kids_male, num_kids_female,
    kid_age_min, kid_age_max, kid_age_mean,
    n_kid_aged_0_2, n_kid_aged_3_5, n_kid_aged_6_10,
    n_kid_aged_11_13, n_kid_aged_14_17) |>
  pivot_wider(
    # keep all the household-level stuff: kids, region, serial
    id_cols = c(serial, dgorpaf, num_kids_total, num_kids_male, num_kids_female,
                kid_age_min, kid_age_max, kid_age_mean,
                n_kid_aged_0_2, n_kid_aged_3_5, n_kid_aged_6_10,
                n_kid_aged_11_13, n_kid_aged_14_17),
    names_from = sex_tag,
    values_from = all_of(vars_to_suffix),
    names_sep = "_") 

# deviations from means of household-level characteristics
# interaction terms
# regional wealth deviation (dgorpaf)
