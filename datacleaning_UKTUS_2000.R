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
  rename(pnum = sn3, DiaryDay_Act = sn4, ddayw = ddayofwk,
         IMonth = dmonth, IYear = dyear, whatdoing = act1, What_Oth1 = act2) |>
  
  # make weekend identifier
  mutate(is_weekend = ddayw > 5)
  

# only keep information collected at the same point as the time use data
diarymonth_households_2000 = unique_interview_months(data_activities_2000_long)

# find time diaries and number of diaries everyone completes
individual_diaries_2000 = unique_interview_diaries(data_activities_2000_long)

# finding time use
activity_summaries_2000 = data_activities_2000_long |>
  select(serial, pnum, is_weekend, IMonth, IYear, eptime, whatdoing, What_Oth1,
         wit0, wit1, wit2, wit3, wit4, wit5, wit6) |>
  
  # secondary activities and stuff
  mutate(
    
    # primary activity
    activity1_is_leisure = whatdoing %in% leisure_actlines,
    activity1_is_leisure_r = whatdoing %in% restrict_actlines,
    activity1_is_sleep = whatdoing %in% sleep_actlines,
    activity1_is_personalcare = whatdoing %in% personal_care_actlines,
    activity1_is_personalcare_sleep = whatdoing %in% sleep_personalcare,
    activity1_is_childcare = whatdoing %in% childcare_actlines,
    activity1_is_work = whatdoing %in% work_actlines,
    activity1_is_domestic = whatdoing %in% domestic_actlines,
    
    # secondary activity
    activity2_is_leisure = What_Oth1 %in% leisure_actlines,
    activity2_is_leisure_r = What_Oth1 %in% restrict_actlines,
    activity2_is_sleep = What_Oth1 %in% sleep_actlines,
    activity2_is_personalcare = What_Oth1 %in% personal_care_actlines,
    activity2_is_personalcare_sleep = What_Oth1 %in% sleep_personalcare,
    activity2_is_childcare = What_Oth1 %in% childcare_actlines,
    activity2_is_work = What_Oth1 %in% work_actlines,
    activity2_is_domestic = What_Oth1 %in% domestic_actlines,
    
    # general: is leisure?
    activity_is_leisure = (activity1_is_leisure | activity2_is_leisure),
    activity_is_leisure_r = (activity1_is_leisure_r | activity2_is_leisure_r),
    
    # general: is sleep (only)?
    activity_is_sleep = (activity1_is_sleep | activity2_is_sleep),
    
    # general: is personal care (only)?
    activity_is_personalcare = (activity1_is_personalcare | 
                                  activity2_is_personalcare),
    
    # general: is personal care OR sleep?
    activity_is_personalcare_sleep = (activity1_is_personalcare_sleep | 
                                        activity2_is_personalcare_sleep),
    
    # general: is leisure? (trying something)
    # activity_is_leisure = activity1_is_leisure,
    # activity_is_leisure_r = activity1_is_leisure_r,
    

    # private activity classifier (no relevant household members present)
    # activities where "who" isn't asked are considered private
    activity_private = (wit1 == 0 & wit2 == 0 & wit3 == 0),
    # spouse not present (as equivalent as possible)
    activity_excludesspouse = wit3 == 0,
    
    # note that sleep doesn't have accompanying copresence information
    # so i will just classify it as private
    # general: is private leisure?
    private_leisure = (activity_is_leisure & activity_private)| activity_is_sleep, 
    private_leisure_r = activity_is_leisure_r & activity_private,
    
    # general: is childcare?
    activity_ischildcare = (activity1_is_childcare | activity2_is_childcare | 
                              wit1 == 1 | wit2 == 1),
    
    # general: is work?
    activity_iswork = (activity1_is_work | activity2_is_work),
    
    # general: is domestic?
    activity_isdomestic = (activity1_is_domestic | activity2_is_domestic),
    
    # is no-spouse childcare?
    childcare_nospouse = activity_ischildcare & activity_excludesspouse,
    
    # is no-spouse domestic?
    domestic_nospouse = activity_isdomestic & activity_excludesspouse,
    
    ) |>
  
  # add up time use in hours
  group_by(serial, pnum, is_weekend) |>
  summarise(
    total_leisure = sum(eptime[activity_is_leisure], na.rm = TRUE) / 60,
    total_private_leisure = sum(eptime[private_leisure], na.rm = TRUE) / 60,
    total_leisure_r = sum(eptime[activity_is_leisure_r], na.rm = TRUE) / 60,
    total_private_leisure_r = sum(eptime[private_leisure_r], na.rm = TRUE) / 60,
    
    total_childcare = sum(eptime[activity_ischildcare], na.rm = TRUE)  / 60,
    total_childcare_nospouse = sum(eptime[childcare_nospouse], na.rm = TRUE) / 60,
    
    total_domestic = sum(eptime[activity_isdomestic], na.rm = TRUE)  / 60,
    total_domestic_nospouse = sum(eptime[domestic_nospouse], na.rm = TRUE) / 60,
    
    total_work = sum(eptime[activity_iswork], na.rm = TRUE)  / 60,
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

# pay band midpoints (monthly midpoints for q10x bands 0–10)
q10x_mid = c(108, 325, 650, 1083, 1516, 2275, 3120, 3600, 4200, 5625, 7000)

# get working straight couples with valid education, and then split into
# parents and non-parents
data_working_couples_2000 = data_individual_2000 |>

  # keep only valid education
  filter(!is.na(educ)) |>

  # show number of diaries and only keep diary month
  inner_join(individual_diaries_2000, by = c("serial", "pnum")) |>
  # merge with time use
  inner_join(activity_summaries_2000, by = c("serial", "pnum")) |>

  # identify couples
  inner_join(spouse_pairs_2000, by = c("serial", "pnum")) |>

  # keep only people who fill out both diaries |>
  filter(num_diaries_filled == 2) |>

  # dealing with all different wage/hours related variables

  # working hours (paid hours only, used for converting pay to hourly wage)
  mutate(
    emp_hours = case_when(
      q14b == 2 & q14c > 0 ~ as.numeric(q14c),
      q14b == 1 & q14d > 0 ~ as.numeric(q14d) + if_else(q14e > 0, as.numeric(q14e), 0),
      TRUE ~ NA_real_)) |>

  # for employees
  mutate(
    # convert pay to weekly for standard periods (q11 1-5)
    emp_pay_weekly_exact = case_when(
      q7 == 1 & q10 >= 0 & q11 %in% 1:5 ~ pay_to_weekly(q10, q11),
      TRUE ~ NA_real_),

    # if paid by hours (q11 == 8), q10 / q11n gives direct hourly rate
    emp_wage_exact = case_when(
      q7 == 1 & q10 >= 0 & q11 == 8 & q11n > 0 ~ q10 / q11n,
      q7 == 1 & !is.na(emp_pay_weekly_exact) & emp_hours > 0 ~ emp_pay_weekly_exact / emp_hours,
      TRUE ~ NA_real_),

    # banded fallback: q10x band index -> midpoint -> weekly -> hourly
    pay_banded = if_else(q7 == 1 & q10 < 0 & q10x %in% 0:10, q10x_mid[q10x + 1], NA_real_),
    emp_pay_weekly_banded = pay_to_weekly(pay_banded, q11),
    emp_wage_banded = if_else(
      q7 == 1 & is.na(emp_wage_exact) & !is.na(emp_pay_weekly_banded) & emp_hours > 0,
      emp_pay_weekly_banded / emp_hours,
      NA_real_)) |>

  # for self employed
  mutate(
    se_pay_weekly_exact = case_when(
      q7 == 2 & q13c > 0 ~ q13c / 4.333,
      TRUE ~ NA_real_),

    # banded fallback: q13d is 1-indexed (1=<£215 ... 11=£6670+), same bands as q10x
    se_pay_monthly_banded = if_else(q7 == 2 & q13c <= 0 & q13d %in% 1:11,
                                    q10x_mid[q13d], NA_real_),
    se_pay_weekly_banded = se_pay_monthly_banded / 4.333,

    se_wage_exact = case_when(
      q7 == 2 & !is.na(se_pay_weekly_exact) & emp_hours > 0 ~ se_pay_weekly_exact / emp_hours,
      TRUE ~ NA_real_),

    se_wage_banded = if_else(
      q7 == 2 & is.na(se_wage_exact) & !is.na(se_pay_weekly_banded) & emp_hours > 0,
      se_pay_weekly_banded / emp_hours,
      NA_real_)) |>

  # source of wage
  mutate(
    wage_source = case_when(
      !is.na(emp_wage_exact) ~ "employee_exact",
      !is.na(emp_wage_banded) ~ "employee_banded",
      !is.na(se_wage_exact) ~ "self_employed_exact",
      !is.na(se_wage_banded) ~ "self_employed_banded",
      TRUE ~ NA_character_),

    NetWkly = case_when(
      wage_source == "employee_exact" ~ emp_pay_weekly_exact,
      wage_source == "employee_banded" ~ emp_pay_weekly_banded,
      wage_source == "self_employed_exact" ~ se_pay_weekly_exact,
      wage_source == "self_employed_banded" ~ se_pay_weekly_banded,
      TRUE ~ NA_real_),

    wage = case_when(
      wage_source == "employee_exact" ~ emp_wage_exact,
      wage_source == "employee_banded" ~ emp_wage_banded,
      wage_source == "self_employed_exact" ~ se_wage_exact,
      wage_source == "self_employed_banded" ~ se_wage_banded,
      TRUE ~ NA_real_)) |>

  # actual hours worked (incl. unpaid overtime), kept for HrWkAc in vars_to_suffix
  mutate(
    q14c_c = if_else(q14c > 0, as.numeric(q14c), NA_real_),
    q14d_c = if_else(q14d > 0, as.numeric(q14d), NA_real_),
    q14e_c = if_else(q14e > 0, as.numeric(q14e), 0),
    q14f_c = if_else(q14f > 0, as.numeric(q14f), 0),

    HrWkAc = case_when(
      q14b == 2 ~ q14c_c,
      q14b == 1 ~ q14d_c + q14e_c + q14f_c,
      TRUE ~ NA_real_)) |>
  
  filter(wage > 0, wage < 100) |>

  group_by(serial) |>
  
  # keep any households where both people are captured by this hourly wage
  filter(all(wage > 0)) |>
  # deciding to keep employees only
  # filter(wage_source == "employee_exact") |>
  
  # check for couples who both have time diaries (filter after both joins)
  mutate(spouse_present = spouse_pnum %in% pnum) |>
  
  ungroup() |> 
  
  # only keep couples who both have time diaries (filter after joins)
  filter(spouse_present) |>
  
  # indicate whether someone is the spouse
  mutate(is_spouse = !is_resp) |>
  
  # make sure time use EXPENDITURE is calculated across all days
  group_by(serial, pnum, is_weekend) |>
  # individual expenditure calculated using time use
  mutate(# leisure and childcare expenditure
    total_leisure_exp = wage * sum(total_leisure),
    total_leisure_exp_r = wage * sum(total_leisure_r),
    private_leisure_exp = wage * sum(total_private_leisure),
    private_leisure_exp_r = wage * sum(total_private_leisure_r),
    total_childcare_exp = wage * sum(total_childcare),
    nospouse_childcare_exp = wage * sum(total_childcare_nospouse),
    # individual contribution to household budget
    y_individual = wage * 24) |>
  ungroup()

# parents
data_working_parents_2000 = data_working_couples_2000 |>
  
  # keep only hetero couples with child in household, valid education
  filter(hhtype4 %in% c(4,5,7,8)) |>
  # merge kid information 
  inner_join(kids_counts_2000, by = c("serial")) |>
  inner_join(kids_age_dist_2000, by = c("serial")) |>
  inner_join(kids_age_wide_2000, by = c("serial")) |>
  
  group_by(serial) |>
  
  # check for couples who both have time diaries (filter after both joins)
  mutate(spouse_present = spouse_pnum %in% pnum) |>
  
  ungroup() |> 
  
  # only keep couples who both have time diaries (filter after both joins)
  filter(spouse_present) |>
  
  # keep households with under-18 kids
  filter(kid_age_min < 18)

# get non-parent couples (including empty nesters: hhtype4 5/8 with all kids >= 18)
data_working_nonparents_2000 = data_working_couples_2000 |>
  # hhtype4 3/6: no children; hhtype4 5/8: children all >= 16 (check below for >= 18)
  filter(hhtype4 %in% c(3, 5, 6, 8)) |>
  # left join to get youngest kid age; NA means no children in household
  left_join(kids_age_dist_2000 |> select(serial, kid_age_min), by = "serial") |>
  # keep childless couples and empty nesters (youngest child >= 18)
  filter(hhtype4 %in% c(3, 6) | kid_age_min >= 18) |>
  select(-kid_age_min) |>

  group_by(serial) |>

  # check for couples who both have time diaries (filter after both joins)
  mutate(spouse_present = spouse_pnum %in% pnum) |>

  ungroup() |>

  # only keep couples who both have time diaries (filter after joins)
  filter(spouse_present)



################################################################################
##################### CALCULATING HOUSEHOLD CHARACTERISTICS ####################
################################################################################

parents_est_data_2000 = data_working_parents_2000 |>
  zap_labels() |>
  # letter for creating variable names
  mutate(sex_tag = if_else(male, "m", "f")) |>
  select(
    serial, is_weekend, sex_tag, dgorpaf, all_of(vars_to_suffix),
    # child info (household-level already, duplicated across spouses)
    num_kids_total, num_kids_male, num_kids_female,
    kid_age_min, kid_age_max, kid_age_mean,
    n_kid_aged_0_2, n_kid_aged_3_5, n_kid_aged_6_10,
    n_kid_aged_11_13, n_kid_aged_14_17) |>
  pivot_wider(
    # keep all the household-level stuff: kids, region, serial
    id_cols = c(serial, is_weekend, dgorpaf, num_kids_total, 
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
    private_leisure_exp_f = private_leisure_exp_f * deflator_2000,
    
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
         Bx_dev_gdppc = y * dev_gdppc) |>
  
  # since the expenditure variables are calculated across the entire survey 
  # period, it doesn't matter which one i drop
  group_by(serial) |>
  filter(!is_weekend) |>
  ungroup() |>
  select(!is_weekend)

# work in progress! 
nonparents_est_data_2000 = data_working_nonparents_2000 |>
  zap_labels() |>
  # letter for creating variable names
  mutate(sex_tag = if_else(male, "m", "f")) |>
  select(
    serial, is_weekend, sex_tag, dgorpaf, all_of(vars_to_suffix)) |>
  pivot_wider(
    # keep all the household-level stuff: kids, region, serial
    id_cols = c(serial, is_weekend, dgorpaf),
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
    private_leisure_exp_f = private_leisure_exp_f * deflator_2000,
    
    private_leisure_exp_r_m = private_leisure_exp_r_m * deflator_2000,
    private_leisure_exp_r_f = private_leisure_exp_r_f * deflator_2000,
    
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
         Bx_dev_gdppc = y * dev_gdppc) |>
  
  # since the expenditure variables are calculated across the entire survey 
  # period, it doesn't matter which one i drop
  group_by(serial) |>
  filter(!is_weekend) |>
  ungroup() |>
  select(!is_weekend)