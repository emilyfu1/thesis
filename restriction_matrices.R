library(tidyverse)
source("functions.R")

regs_within = c("Bx_dev_wage_f_only", "Bx_dev_wage_m_only",
                "Bx_dev_educ_f_only", "Bx_dev_educ_m_only",
                "Bx_dev_avgage", "Bx_dev_agegap", "Bx_dev_gdppc")

theta_names_within = c("theta_wf", "theta_wm", "theta_ef", "theta_em",
                       "theta_age", "theta_agegap", "theta_regwealth")

modReg_within = make_regMat(regressors = regs_within,
                            theta_names = theta_names_within)

regs_between = c("Bx_dev_wage_f_all", "Bx_dev_wage_m_all",
                 "Bx_dev_educ_f_all", "Bx_dev_educ_m_all",
                 "Bx_dev_avgage", "Bx_dev_agegap", "Bx_dev_gdppc")

theta_names_between = c("theta_wf", "theta_wm", "theta_ef", "theta_em",
                        "theta_age", "theta_agegap", "theta_regwealth")

modReg_between = make_regMat(regressors = regs_between,
                             theta_names = theta_names_between)

regs_both = c("Bx_dev_wage_f_all", "Bx_dev_wage_m_all",
              "Bx_dev_educ_f_all", "Bx_dev_educ_m_all",
              "Bx_dev_wage_f_only", "Bx_dev_wage_m_only",
              "Bx_dev_educ_f_only", "Bx_dev_educ_m_only",
              "Bx_dev_avgage", "Bx_dev_agegap", "Bx_dev_gdppc")

theta_names_both = c("theta_wf_between", "theta_wm_between", "theta_ef_between", 
                     "theta_em_between", "theta_wf_within", "theta_wm_within", 
                     "theta_ef_within", "theta_em_within", "theta_age", 
                     "theta_agegap", "theta_regwealth")

modReg_both = make_regMat(regressors = regs_both,
                          theta_names = theta_names_both)