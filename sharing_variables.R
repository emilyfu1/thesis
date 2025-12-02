library(tidyverse)
setwd("/Users/emilyfu/Desktop/school/thesis")

# import data
final_individual_data = read.csv("atus_working_parents_act.csv")

# household level (since now there's a proper separate variable for respondent)
hh = final_individual_data |>
  group_by(YEAR, SERIAL) |>   # unique household identifier
  summarise(
    # household leisure expenditure by gender
    leisure_exp_f = mean(leisure_exp_f, na.rm=TRUE),
    leisure_exp_m = mean(leisure_exp_m, na.rm=TRUE),
    
    # total household budget
    y = mean(y, na.rm=TRUE),
    
    # sex-specific variables
    wage_f = mean(wage_f, na.rm=TRUE),
    wage_m = mean(wage_m, na.rm=TRUE),
    educ_f = mean(educ_f, na.rm=TRUE),
    educ_m = mean(educ_m, na.rm=TRUE),
    
    # age variables (your grouped variables already computed these correctly)
    avg_age = mean(avg_age, na.rm=TRUE),
    age_gap = mean(age_gap, na.rm=TRUE),
    
    # include deviations
    dev_wage_f = mean(dev_wage_f, na.rm=TRUE),
    dev_wage_m = mean(dev_wage_m, na.rm=TRUE),
    dev_educ_f = mean(dev_educ_f, na.rm=TRUE),
    dev_educ_m = mean(dev_educ_m, na.rm=TRUE),
    dev_avg_age = mean(dev_avgage, na.rm=TRUE),
    dev_age_gap = mean(dev_agegap, na.rm=TRUE),
    
    # interaction terms for the SUR
    Bx_dev_wage_f = mean(Bx_dev_wage_f, na.rm=TRUE),
    Bx_dev_wage_m = mean(Bx_dev_wage_m, na.rm=TRUE),
    Bx_dev_educ_f = mean(Bx_dev_educ_f, na.rm=TRUE),
    Bx_dev_educ_m = mean(Bx_dev_educ_m, na.rm=TRUE),
    Bx_dev_avgage = mean(Bx_dev_avgage, na.rm=TRUE),
    Bx_dev_agegap = mean(Bx_dev_agegap, na.rm=TRUE),
    
    .groups = "drop")

# save
write.csv(hh,"sharing_est.csv", row.names = FALSE)