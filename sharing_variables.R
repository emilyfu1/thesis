library(tidyverse)
setwd("/Users/emilyfu/Desktop/school/thesis")

# import data
final_individual_data = read.csv("atus_working_parents_act.csv")

# generate variables for sharing estimates
final_individual_data = final_individual_data |> 
  mutate(hrly_wage = EARNWEEK / UHRSWORKT, # hourly wage
         # time use in hours
         total_leisure_h = total_leisure / 60,
         total_private_leisure_h = total_private_leisure / 60,
         total_childcare_h = total_childcare / 60,
         total_childcare_nospouse_h = total_childcare_nospouse / 60,
         # private leisure expenditure
         leisure_exp = hrly_wage * total_private_leisure_h,
         # education in levels
         educ_cat = case_when(EDUCYRS < 200 ~ 0, # below high school completion
                              # high school to some college
                              EDUCYRS >= 200 & EDUCYRS < 217 ~ 1,
                              # completed undergraduate and graduate degrees
                              EDUCYRS >= 217 & EDUCYRS <= 321 ~ 2,
                              TRUE ~ NA_real_)) |>
  
  # household-level constructs
  group_by(YEAR, SERIAL) |> 
  mutate(hh_leisure_budget = sum(leisure_exp, na.rm=TRUE),
         avg_age = mean(AGE),
         age_gap = abs(diff(AGE))) |> 
  ungroup() |>
  
  # since it's a pooled sample? 
  group_by(YEAR) |>
  # deviations from mean
  mutate(dev_hrly_wage = hrly_wage - mean(hrly_wage, na.rm=TRUE),
         dev_avg_age = avg_age - mean(avg_age, na.rm=TRUE),
         dev_age_gap = age_gap- mean(age_gap, na.rm=TRUE),) |> 
  ungroup() |>
  # interaction terms
  mutate(Bx_dev_wage = hh_leisure_budget * dev_hrly_wage,
         Bx_dev_educ = hh_leisure_budget * dev_educ,
         Bx_dev_avgage = hh_leisure_budget * dev_avg_age,
         Bx_dev_agegap = hh_leisure_budget * dev_age_gap)