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
         DVAge = iage, dgorpaf = gorpaf) |>
  
  # make region codes align with 2015 data
  mutate(dgorpaf = if_else(dgorpaf >= 9, dgorpaf + 1, dgorpaf)) |>
  
  # make gender and respondent dummies
  mutate(is_resp = pnum == hrp_per,
         male = DMSex == 1) |>
  
  # keep if observation has age, sex
  filter(DMSex != 3, DVAge >= 0) |>
  mutate(# Highest qualification obtained, simplified into three categories. 
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

spouse_pairs_2000 = find_spouse_pairs(all_relationships_2000)

# pay band midpoints
q10x_mid = c(108, 325, 650, 1083, 1516, 2275, 3120, 3600, 4200, 5625, 7000)

# parents
data_working_parents_2000 = data_individual_2000 |>
  
  # keep only hetero couples with child in household, valid education
  filter(hhtype4 %in% c(4,5,7,8), !is.na(educ)) |>
  
  # show number of diaries and only keep diary month
  inner_join(individual_diaries_2000, by = c("serial", "pnum")) |>
  # merge with time use
  inner_join(activity_summaries_2000, by = c("serial", "pnum")) |>
  
  # merge kid information 
  inner_join(kids_counts_2000, by = c("serial")) |>
  inner_join(kids_age_dist_2000, by = c("serial")) |>
  inner_join(kids_age_wide_2000, by = c("serial")) |>
  
  # identify couples
  inner_join(spouse_pairs_2000, by = c("serial", "pnum")) |>
  
  # dealing with all different wage/hours related variables  
  
  # prepare working hours stuff
  mutate(
    weekly_hours = case_when(
      q14b == 2 & q14c > 0 ~ q14c,
      q14b == 1 & q14d > 0 ~ q14d + if_else(q14e > 0, q14e, 0),
      TRUE ~ NA_real_)) |> 
  
  # for employees
  mutate(
    NetWkly = if_else(
      q7 == 1 & q10 >= 0,
      pay_to_weekly(q10, q11),
      NA_real_),
    hourly_wage_emp = NetWkly / weekly_hours,
    pay_banded = if_else(q10 < 0 & q10x %in% 0:10, q10x_mid[q10x + 1], NA_real_),
    NetWkly_banded = pay_to_weekly(pay_banded, q11),
    hourly_wage_emp_banded = if_else(
      is.na(hourly_wage_emp),
      NetWkly_banded / weekly_hours,
      hourly_wage_emp)) |>
  
  # for self employed
  mutate(
    hourly_earnings_selfemp =
      if_else(
        q7 == 2 & q13c > 0 & weekly_hours > 0,
        q13c / (4.333 * weekly_hours),
        NA_real_)) |>
  
  # source of wage
  mutate(
    wage_source = case_when(
      !is.na(hourly_wage_emp) ~ "employee_exact",
      !is.na(hourly_wage_emp_banded) ~ "employee_banded",
      !is.na(hourly_earnings_selfemp) ~ "self_employed",
      TRUE ~ NA_character_)) |>
  
  # combine wages
  mutate(
    NetWkly = case_when(
      wage_source == "employee_exact" ~ NetWkly,
      wage_source == "employee_banded" ~ NetWkly_banded,
      wage_source == "self_employed" ~ q13c / 4.333,
      TRUE ~ NA_real_),
    wage = case_when(
      wage_source == "employee_exact" ~ hourly_wage_emp,
      wage_source == "employee_banded" ~ hourly_wage_emp_banded,
      wage_source == "self_employed" ~ hourly_earnings_selfemp,
      TRUE ~ NA_real_)) |>
  
  # combine working hours
  mutate(
    # clean hours pieces
    q14c_c = if_else(q14c > 0, as.numeric(q14c), NA_real_),
    q14d_c = if_else(q14d > 0, as.numeric(q14d), NA_real_),
    q14e_c = if_else(q14e > 0, as.numeric(q14e), 0),  # overtime default 0
    q14f_c = if_else(q14f > 0, as.numeric(q14f), 0),
    
    HrWkAc = case_when(
      q14b == 2 ~ q14c_c,                    # no overtime
      q14b == 1 ~ q14d_c + q14e_c + q14f_c,  # overtime
      TRUE ~ NA_real_
    )) |>
  
  group_by(serial) |>
  
  # keep any households where both people are captured by this hourly wage
  filter(all(wage > 0)) |>
  # deciding to keep employees only
  # filter(wage_source == "employee_exact") |>
  
  # check for couples who both have time diaries (filter after both joins)
  mutate(spouse_present = spouse_pnum %in% pnum) |>
  
  ungroup() |> 
  
  # only keep couples who both have time diaries (filter after both joins)
  filter(spouse_present) |>
  
  # indicate whether someone is the spouse
  mutate(is_spouse = !is_resp) |>
  
  # individual expenditure calculated using time use
  mutate(# leisure and childcare expenditure
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

sharing_est_data_2000 = data_working_parents_2000 |>
  zap_labels() |>
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
    id_cols = c(serial, dgorpaf, num_kids_total, 
                num_kids_male, num_kids_female,
                kid_age_min, kid_age_max, kid_age_mean,
                n_kid_aged_0_2, n_kid_aged_3_5, n_kid_aged_6_10,
                n_kid_aged_11_13, n_kid_aged_14_17),
    names_from = sex_tag,
    values_from = all_of(vars_to_suffix),
    names_sep = "_") |>
  inner_join(regionalwealth_2000, by = c("dgorpaf")) |>
  
  # fill in annual income, household budget, average age, age gap
  mutate(income_annual = (NetWkly_f + NetWkly_m)*52,
         y = y_individual_f + y_individual_m,
         avgage = (DVAge_f + DVAge_m)/2,
         agegap_m = DVAge_m - DVAge_f) |>
  
  # deflate all monetary variables
  mutate(
    # household budget
    y = y * deflator_2000,
    y_individual_m = y_individual_m * deflator_2000,
    y_individual_f = y_individual_f * deflator_2000,
    
    # wages
    NetWkly_m = NetWkly_m * deflator_2000,
    NetWkly_f = NetWkly_f * deflator_2000,
    wage_m = wage_m * deflator_2000,
    wage_f = wage_f * deflator_2000,
    
    # leisure expenditure
    total_leisure_exp_m = total_leisure_exp_m * deflator_2000,
    total_leisure_exp_f = total_leisure_exp_f * deflator_2000,
    
    total_leisure_exp_r_m = total_leisure_exp_r_m * deflator_2000,
    total_leisure_exp_r_f = total_leisure_exp_r_f * deflator_2000,
    
    private_leisure_exp_m = private_leisure_exp_m * deflator_2000,
    private_leisure_exp_m = private_leisure_exp_m * deflator_2000,
    
    private_leisure_exp_r_m = private_leisure_exp_r_m * deflator_2000,
    private_leisure_exp_r_f = private_leisure_exp_r_f * deflator_2000,
    
    # childcare
    total_childcare_exp_m = total_childcare_exp_m * deflator_2000,
    total_childcare_exp_f = total_childcare_exp_f * deflator_2000,
    
    nospouse_childcare_exp_m = nospouse_childcare_exp_m * deflator_2000,
    nospouse_childcare_exp_f = nospouse_childcare_exp_f * deflator_2000,
    
    # regional wealth
    income_annual = income_annual * deflator_2000) |>
  
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
    dev_gdppc = income_annual - rgdppc) |>
  
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

write_csv(sharing_est_data_2000, paste0(data_direct, "sharing_est_data_2000.csv"))
