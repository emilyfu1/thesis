library(tidyverse)
setwd("/Users/emilyfu/Desktop/school/thesis")

# import data
final_individual_data = read.csv("atus_working_parents_act.csv")

# generate variables for sharing estimates
final_individual_data = final_individual_data |> 
  mutate(hrly_wage = EARNWEEK / UHRSWORKT, # hourly wage
         # spouse hourly wage
         spouse_hrly_wage = spouse_earnweek / spouse_usualhours,
         # time use in hours
         total_leisure_h = total_leisure / 60,
         total_private_leisure_h = total_private_leisure / 60,
         total_childcare_h = total_childcare / 60,
         total_childcare_nospouse_h = total_childcare_nospouse / 60,
         # private leisure expenditure
         leisure_exp = hrly_wage * total_private_leisure_h,
         # education in levels
         educ_cat = case_when(EDUCYRS < 200 ~ 0, # below high school
                              # high school to some college
                              EDUCYRS >= 200 & EDUCYRS < 217 ~ 1,
                              # completed undergraduate/graduate degrees
                              EDUCYRS >= 217 & EDUCYRS <= 321 ~ 2,
                              TRUE ~ NA_real_),
         # spouse education in levels
         spouse_educ_cat = case_when(spouse_educ < 200 ~ 0, # below high school
                                  # high school to some college
                                  spouse_educ >= 200 & spouse_educ < 217 ~ 1,
                                  # completed undergraduate/graduate degrees
                                  spouse_educ >= 217 & spouse_educ <= 321 ~ 2,
                                  TRUE ~ NA_real_),
         # sex specific wages
         wage_f = if_else(SEX == 2, hrly_wage, spouse_hrly_wage),
         wage_m = if_else(SEX == 1, hrly_wage, spouse_hrly_wage),
         educ_f = if_else(SEX == 2, educ_cat, spouse_educ_cat),
         educ_m = if_else(SEX == 1, educ_cat, spouse_educ_cat)) |>
  
  # household-level constructs
  group_by(YEAR, SERIAL) |>
  mutate(avg_age = mean(AGE),
         age_gap = diff(AGE),
         # the household budget
         y = (wage_f + wage_m) * 24,
         
         # leisure expenditure
         leisure_exp_f = sum(leisure_exp[SEX==2]),
         leisure_exp_m = sum(leisure_exp[SEX==1])) |> 
  ungroup() |>
  
  # since it's a pooled sample? 
  group_by(YEAR) |>
  # deviations from mean
    mutate(dev_wage_f = wage_f - mean(wage_f, na.rm=TRUE),
           dev_wage_m = wage_m - mean(wage_m, na.rm=TRUE),
           dev_educ_f = educ_f - mean(educ_f, na.rm=TRUE),
           dev_educ_m = educ_m - mean(educ_m, na.rm=TRUE),
           dev_avg_age = avg_age - mean(avg_age, na.rm=TRUE),
           dev_age_gap = age_gap - mean(age_gap, na.rm=TRUE)) |> 
  ungroup() |>
  # interaction terms
  mutate(Bx_dev_wage_f = y * dev_wage_f,
         Bx_dev_wage_m = y * dev_wage_m,
         Bx_dev_educ_f = y * dev_educ_f,
         Bx_dev_educ_m = y * dev_educ_m,
         Bx_dev_age = y * dev_avg_age,
         Bx_dev_age_gap = y * dev_age_gap)

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
    dev_avg_age = mean(dev_avg_age, na.rm=TRUE),
    dev_age_gap = mean(dev_age_gap, na.rm=TRUE),
    
    # interaction terms for the SUR
    Bx_dev_wage_f = mean(Bx_dev_wage_f, na.rm=TRUE),
    Bx_dev_wage_m = mean(Bx_dev_wage_m, na.rm=TRUE),
    Bx_dev_educ_f = mean(Bx_dev_educ_f, na.rm=TRUE),
    Bx_dev_educ_m = mean(Bx_dev_educ_m, na.rm=TRUE),
    Bx_dev_age = mean(Bx_dev_age, na.rm=TRUE),
    Bx_dev_age_gap = mean(Bx_dev_age_gap, na.rm=TRUE),
    
    .groups = "drop")

# save
write.csv(final_individual_data,"atus_working_parents_act.csv", 
          row.names = FALSE)
write.csv(hh,"sharing_est.csv", row.names = FALSE)