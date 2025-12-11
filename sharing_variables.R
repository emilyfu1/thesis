library(tidyverse)
setwd("/Users/emilyfu/Desktop/school/thesis")

# import data
final_individual_data = read.csv("atus_working_parents_act.csv")

# household level (since now there's a proper separate variable for respondent)
hh = final_individual_data |>
  group_by(YEAR, SERIAL) |>   # unique household identifier
  summarise(
    # household state and county fip codes
    STATEFIP = max(STATEFIP, na.rm=TRUE),
    county = max(COUNTY, na.rm=TRUE),
    
    # household id
    SERIAL = max(SERIAL, na.rm=TRUE),
    
    # child dummies
    KID1TO2 = max(KID1TO2, na.r=TRUE),
    KID3TO5 = max(KID3TO5, na.r=TRUE),
    KID6TO12 = max(KID6TO12, na.r=TRUE),
    KID13TO17 = max(KID13TO17, na.r=TRUE),
    
    # household leisure expenditure by gender
    leisure_exp_f = max(leisure_exp_f, na.rm=TRUE),
    leisure_exp_m = max(leisure_exp_m, na.rm=TRUE),
    leisure_exp_f_r = max(leisure_exp_f_r, na.rm=TRUE),
    leisure_exp_m_r = max(leisure_exp_m_r, na.rm=TRUE),
    total_childcare_nospouse_h_f = max(total_childcare_nospouse_h_f),
    total_childcare_nospouse_h_m = max(total_childcare_nospouse_h_m),
    
    # total household budget
    y = max(y, na.rm=TRUE),
    
    # sex-specific variables
    wage_f = max(wage_f, na.rm=TRUE),
    wage_m = max(wage_m, na.rm=TRUE),
    educ_f = max(educ_f, na.rm=TRUE),
    educ_m = max(educ_m, na.rm=TRUE),
    race_f = max(race_f, na.rm=TRUE),
    race_m = max(race_m, na.rm=TRUE),
    race_f = max(race_f, na.rm=TRUE),
    race_m = max(race_m, na.rm=TRUE),
    uhrsworkt_f = max(uhrsworkt_f, na.rm=TRUE),
    uhrsworkt_m = max(uhrsworkt_m, na.rm=TRUE),
    
    # age variables (your grouped variables already computed these correctly)
    avg_age = max(avg_age, na.rm=TRUE),
    age_gap = max(age_gap, na.rm=TRUE),
    
    # include deviations
    dev_wage_f_only = max(dev_wage_f_only, na.rm=TRUE),
    dev_wage_m_only = max(dev_wage_m_only, na.rm=TRUE),
    dev_educ_f_only = max(dev_educ_f_only, na.rm=TRUE),
    dev_educ_m_only = max(dev_educ_m_only, na.rm=TRUE),
    dev_wage_f_all = max(dev_wage_f_all, na.rm=TRUE),
    dev_wage_m_all = max(dev_wage_m_all, na.rm=TRUE),
    dev_educ_f_all = max(dev_educ_f_all, na.rm=TRUE),
    dev_educ_m_all = max(dev_educ_m_all, na.rm=TRUE),
    dev_avg_age = max(dev_avgage, na.rm=TRUE),
    dev_age_gap = max(dev_agegap, na.rm=TRUE),
    
    # interaction terms for the SUR
    Bx_dev_wage_f_only = max(Bx_dev_wage_f_only, na.rm=TRUE),
    Bx_dev_wage_m_only = max(Bx_dev_wage_m_only, na.rm=TRUE),
    Bx_dev_educ_f_only = max(Bx_dev_educ_f_only, na.rm=TRUE),
    Bx_dev_educ_m_only = max(Bx_dev_educ_m_only, na.rm=TRUE),
    Bx_dev_wage_f_all = max(Bx_dev_wage_f_all, na.rm=TRUE),
    Bx_dev_wage_m_all = max(Bx_dev_wage_m_all, na.rm=TRUE),
    Bx_dev_educ_f_all = max(Bx_dev_educ_f_all, na.rm=TRUE),
    Bx_dev_educ_m_all = max(Bx_dev_educ_m_all, na.rm=TRUE),
    Bx_dev_avgage = max(Bx_dev_avgage, na.rm=TRUE),
    Bx_dev_agegap = max(Bx_dev_agegap, na.rm=TRUE),
    
    .groups = "drop")

# save
write.csv(hh,"sharing_est.csv", row.names = FALSE)