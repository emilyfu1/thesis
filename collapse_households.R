library(tidyverse)
setwd(Sys.getenv("THESIS_WD"))

# import data
sharing_est_data = read.csv("atus_working_parents_act.csv")

# household level (since now there's a proper separate variable for each member)
sharing_est_data_collapse = sharing_est_data |>
  arrange(YEAR, SERIAL) |>
  distinct(YEAR, SERIAL, .keep_all = TRUE) |>
  transmute(
    # identifiers
    YEAR, SERIAL, STATEFIP, COUNTY,
    
    # kids
    num_kids_total, num_kids_male, num_kids_female, kid_age_min, kid_age_max, 
    kid_age_mean, n_kid_aged_0_2, n_kid_aged_3_5, n_kid_aged_6_10, 
    n_kid_aged_11_13, n_kid_aged_14_17, age_of_kid_1, age_of_kid_2, 
    age_of_kid_3, age_of_kid_4, age_of_kid_5, age_of_kid_6, age_of_kid_7, 
    age_of_kid_8, age_of_kid_9,
    
    # time use, expenditure, budget
    leisure_exp_f, leisure_exp_m,
    leisure_exp_f_r, leisure_exp_m_r,
    total_childcare_nospouse_h_f, total_childcare_nospouse_h_m,
    y,
    
    # covariates
    wage_f, wage_m,
    educ_f, educ_m,
    race_f, race_m,
    uhrsworkt_f, uhrsworkt_m,
    avgage,
    agegap = agegap_m,
    
    # deviations
    dev_wage_f_only, dev_wage_m_only,
    dev_educ_f_only, dev_educ_m_only,
    dev_wage_f_all, dev_wage_m_all,
    dev_educ_f_all, dev_educ_m_all,
    dev_avgage,
    dev_agegap,
    
    # interactions
    Bx_dev_wage_f_only, Bx_dev_wage_m_only,
    Bx_dev_educ_f_only, Bx_dev_educ_m_only,
    Bx_dev_wage_f_all, Bx_dev_wage_m_all,
    Bx_dev_educ_f_all, Bx_dev_educ_m_all,
    Bx_dev_avgage,
    Bx_dev_agegap)

# save
write.csv(sharing_est_data_collapse,"atus_working_parents_act_collapse.csv", 
          row.names = FALSE)