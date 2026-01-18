library(tidyverse)
library(haven)

# actlines and directories
source("UKTUS_params.R")

# regional wealth
source("regionalwealth.R")

# setwd
setwd(wd)

################################################################################
############################## ACTIVITY-LEVEL DATA #############################
################################################################################

# activity level data
data_activities_2015 = read_dta(paste0(uktus_2015_direct, "uktus15_diary_ep_long.dta"))

# only keep information collected at the same point as the time use data
diarymonth_households_2015 = unique_interview_months(data_activities_2015)

# find time diaries and number of diaries everyone completes
individual_diaries_2015 = unique_interview_diaries(data_activities_2015)

# finding time use
activity_summaries_2015 = data_activities_2015 |>
  select(serial, pnum, daynum, IMonth, IYear, eptime, whatdoing, What_Oth1,
         What_Oth2, What_Oth3, WithAlone, WithSpouse, WithChild, WithOther) |>
  
  # secondary activities and stuff
  mutate(
    activity1_is_leisure = whatdoing %in% leisure_actlines,
    activity1_is_leisure_r = whatdoing %in% restrict_actlines,
    activity1_is_childcare = whatdoing %in% childcare_actlines,
    activity2_is_leisure = What_Oth1 %in% leisure_actlines,
    activity2_is_leisure_r = What_Oth1 %in% restrict_actlines,
    activity2_is_childcare = What_Oth1 %in% childcare_actlines,
    activity3_is_leisure = What_Oth2 %in% leisure_actlines,
    activity3_is_leisure_r = What_Oth2 %in% restrict_actlines,
    activity3_is_childcare = What_Oth2 %in% childcare_actlines,
    activity4_is_leisure = What_Oth3 %in% leisure_actlines,
    activity4_is_leisure_r = What_Oth3 %in% restrict_actlines,
    activity4_is_childcare = What_Oth3 %in% childcare_actlines,
    
    # private (no relevant household members present)
    # activities where "who" isn't asked are considered private
    activity_private = WithSpouse == 0 & WithChild == 0,
    # spouse not present
    activity_excludesspouse = WithSpouse == 0,
    
    # general: is leisure?
    activity_is_leisure = (activity1_is_leisure | activity2_is_leisure | 
                             activity3_is_leisure | activity4_is_leisure),
    activity_is_leisure_r = (activity1_is_leisure_r | activity2_is_leisure_r | 
                               activity3_is_leisure_r | activity4_is_leisure),
    
    # general: is private leisure?
    private_leisure = activity_is_leisure & activity_private,
    private_leisure_r = activity_is_leisure_r & activity_private,
    
    # general: is childcare?
    activity_ischildcare = (activity1_is_childcare | activity2_is_childcare | 
                              activity3_is_childcare | activity4_is_childcare | 
                              WithChild == 1),
    
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
data_hh_2015 = read_dta(paste0(uktus_2015_direct, "uktus15_household.dta"))

################################################################################
############################ INDIVIDUAL-LEVEL DATA #############################
################################################################################

# individual level data
data_individual_2015 = read_dta(paste0(uktus_2015_direct, 
                                       "uktus15_individual.dta")) |>
  # keep if observation has age, sex
  filter(DVAge >= 0, DMSex >= 0) |>
  mutate(male = DMSex == 1, # sex dummy
         is_resp = pnum == SelPer, # indicate respondent
         
         # Highest qualification obtained, simplified into three categories. 
         # Category 2: is equivalent to an undergraduate degree or higher. 
         # Category 1: end-of-school diplomas e.g. A levels, IBDP
         # Category 0: is anything less than that e.g. GSCEs
         educ = case_when(
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
all_relationships_2015 = data_individual_2015 |> 
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
data_kids_2015 = find_kids(relationships_data = all_relationships_2015, 
                           individual_data = data_individual_2015)

# how many kids of each sex
kids_counts_2015 = count_kids(data_kids_2015)

# kid age distribution
kids_age_dist_2015 = find_kid_ages(data_kids_2015)

# get ages of each kid
kids_age_wide_2015 = find_kid_ages_wide(data_kids_2015)

############################## Parents and couples #############################

# find spouse/partner pairs in data
spouse_pairs_2015 = find_spouse_pairs(all_relationships_2015)

# parents
data_working_parents_2015 = data_individual_2015 |>
  # keep only hetero couples with child in household, valid education
  filter(NumSSex == 0, NumChild > 0, !is.na(educ)) |>
  
  # show number of diaries and only keep diary month
  inner_join(individual_diaries_2015, by = c("serial", "pnum")) |>
  inner_join(diarymonth_households_2015, by = c("serial", "pnum")) |>
  # merge with time use
  inner_join(activity_summaries_2015, by = c("serial", "pnum")) |>
  
  # merge kid information 
  inner_join(kids_counts_2015, by = c("serial")) |>
  inner_join(kids_age_dist_2015, by = c("serial")) |>
  inner_join(kids_age_wide_2015, by = c("serial")) |>
  
  # identify couples
  inner_join(spouse_pairs_2015, by = c("serial", "pnum")) |>

  group_by(serial) |>
  
  # based on actual hours worked
  filter(all(NetWkly > 0 | SENetPay > 0), 
         all(HrWkUS > 0 | SEHrWkUs > 0)) |>
  
  # check for couples who both have time diaries (filter after both joins)
  mutate(spouse_present = spouse_pnum %in% pnum) |>

  ungroup() |> 
  
  # only keep couples who both have time diaries (filter after both joins)
  filter(spouse_present) |>
  
  # indicate whether someone is the spouse
  mutate(is_spouse = !is_resp) |>
  
  # combine different wage sources:
  mutate(NetWkly = if_else(NetWkly > 0, NetWkly, SENetPay / 4.33)) |>
  mutate(HrWkUS = if_else(HrWkUS > 0, HrWkUS, SEHrWkUs)) |>
  
  # individual expenditure calculated using time use
  mutate(wage = NetWkly / HrWkUS, # calculated hourly wages
         
         # leisure and childcare expenditure
         total_leisure_exp = wage * total_leisure,
         total_leisure_exp_r = wage * total_leisure_r,
         private_leisure_exp = wage * total_private_leisure,
         private_leisure_exp_r = wage * total_private_leisure_r,
         total_childcare_exp = wage * total_childcare,
         nospouse_childcare_exp = wage * total_childcare_nospouse,
         # individual contribution to household budget
         y_individual = wage * 24 * num_diaries_filled) |>
  
  # keep households with under-18 kids
  filter(kid_age_min < 18)
  
################################################################################
##################### CALCULATING HOUSEHOLD CHARACTERISTICS ####################
################################################################################

sharing_est_data_2015 = data_working_parents_2015 |>
  zap_labels() |>
  # letter for creating variable names
  mutate(sex_tag = if_else(male, "m", "f")) |>
  select(
    serial, sex_tag, dgorpaf, Income, all_of(vars_to_suffix),
    # child info (household-level already, duplicated across spouses)
    num_kids_total, num_kids_male, num_kids_female,
    kid_age_min, kid_age_max, kid_age_mean,
    n_kid_aged_0_2, n_kid_aged_3_5, n_kid_aged_6_10,
    n_kid_aged_11_13, n_kid_aged_14_17) |>
  pivot_wider(
    # keep all the household-level stuff: kids, region, serial
    id_cols = c(serial, dgorpaf, num_kids_total, Income, 
                num_kids_male, num_kids_female,
                kid_age_min, kid_age_max, kid_age_mean,
                n_kid_aged_0_2, n_kid_aged_3_5, n_kid_aged_6_10,
                n_kid_aged_11_13, n_kid_aged_14_17),
    names_from = sex_tag,
    values_from = all_of(vars_to_suffix),
    names_sep = "_") |>
  inner_join(regionalwealth_2014, by = c("dgorpaf")) |>
  
  # fill in annual income, household budget, average age, age gap
  mutate(income_annual = if_else(Income > 0, # treat don't know and refused
                                 Income * 12,
                                 (NetWkly_f + NetWkly_f)*52),
         y = y_individual_f + y_individual_m,
         avgage = (DVAge_f + DVAge_m)/2,
         agegap_m = DVAge_m - DVAge_f) |>
  select(!Income) |>
  
  # deviations from means of household-level characteristics
  mutate(
    # within-sex deviations of education and age
    dev_wage_f_only = wage_f - mean(wage_f, na.rm = TRUE),
    dev_wage_m_only = wage_m - mean(wage_m, na.rm = TRUE),
    dev_educ_f_only = educ_f - mean(educ_f, na.rm = TRUE),
    dev_educ_m_only = educ_m - mean(educ_m, na.rm = TRUE),
    
    # deviations of education and age for both sexes (maybe change this to opposite sexes)
    dev_wage_f_all = wage_f - mean(c(wage_f, wage_m), na.rm = TRUE),
    dev_wage_m_all = wage_m - mean(c(wage_f, wage_m), na.rm = TRUE),
    dev_educ_f_all = educ_f - mean(c(educ_f, educ_m), na.rm = TRUE),
    dev_educ_m_all = educ_m - mean(c(educ_f, educ_m), na.rm = TRUE),
    
    # deviations of education and age from opposite sex
    dev_wage_f_opp = wage_f - mean(wage_m, na.rm = TRUE),
    dev_wage_m_opp = wage_m - mean(wage_f, na.rm = TRUE),
    dev_educ_f_opp = educ_f - mean(educ_m, na.rm = TRUE),
    dev_educ_m_opp = educ_m - mean(educ_f, na.rm = TRUE),
    
    # deviations of average age of couple and age gap
    dev_avgage = avgage - mean(avgage, na.rm = TRUE),
    dev_agegap = agegap_m - mean(agegap_m, na.rm = TRUE),
    
    # deviation of household from regional wealth 
    dev_gdppc = income_annual - ngdppc_2014) |>
  
  # interaction terms
  mutate(Bx_dev_wage_f_only = y * dev_wage_f_only,
         Bx_dev_wage_m_only = y * dev_wage_m_only,
         Bx_dev_educ_f_only = y * dev_educ_f_only,
         Bx_dev_educ_m_only = y * dev_educ_m_only,
         
         Bx_dev_wage_f_all = y * dev_wage_f_all,
         Bx_dev_wage_m_all = y * dev_wage_m_all,
         Bx_dev_educ_f_all = y * dev_educ_f_all,
         Bx_dev_educ_m_all = y * dev_educ_m_all,
         
         Bx_dev_wage_f_opp = y * dev_wage_f_opp,
         Bx_dev_wage_m_opp = y * dev_wage_m_opp,
         Bx_dev_educ_f_opp = y * dev_educ_f_opp,
         Bx_dev_educ_m_opp = y * dev_educ_m_opp,
         
         Bx_dev_avgage = y * dev_avgage,
         Bx_dev_agegap = y * dev_agegap,
         Bx_dev_gdppc = y * dev_gdppc)

write_csv(sharing_est_data_2015, paste0(data_direct, "sharing_est_data_2015.csv"))


