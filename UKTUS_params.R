# import functions
source("functions.R")

# setwd
wd = Sys.getenv("THESIS_WD")
setwd(wd)

# directories
data_direct = paste0(wd,"/uktus_data/")
uktus_2000_direct = paste0(data_direct,"UKDA-4504-stata8_se/stata8_se/")
uktus_2015_direct = paste0(data_direct,"UKDA-8128-stata/stata/stata11_se/")

# dict define region codes to merge with regional wealth
region_codes = new.env()
region_codes$"NORTH EAST" = 1
region_codes$"NORTH WEST" = 2
region_codes$"YORKSHIRE AND THE HUMBER" = 3
region_codes$"EAST MIDLANDS" = 4
region_codes$"WEST MIDLANDS" = 5
region_codes$"EAST" = 6
region_codes$"EASTERN" = 6
region_codes$"LONDON" = 7
region_codes$"SOUTH EAST" = 8
region_codes$"SOUTH WEST" = 10
region_codes$"WALES" = 11
region_codes$"SCOTLAND" = 12
region_codes$"NORTHERN IRELAND" = 13
region_names = ls(region_codes)
region_map = unlist(as.list(region_codes), use.names = TRUE)

################################################################################
################################ ACTIVITY CODES ################################
################################################################################

personal_care_actlines = c(
  ## --- Personal care, rest, eating ---
  0,        # Unspecified personal care
  10,       # Unspecified sleep
  110, 111, # Sleep; Sleep in bed not asleep
  120,      # Sleep: Sick in bed
  210,      # Eating
  5310,     # Resting - time out
  310,      # Other personal care: Wash and dress
  300,      # Other personal care: Unspecified other personal care
  390      # Other personal care: Other specified personal care
  )

restrict_actlines = c(5120, 6149, 6150, 5292, 5130, 6160, 8210,
                      8211, 7190, 6170, 8219, 8221, 8222, 6179, 
                      8229, 6190, 5190, 7241, 5200, 7249, 7251,
                      5210, 1120, 5220, 5221, 5222, 5223, 5225,
                      8300, 5229, 8311, 5245, 8319, 5250, 7300,
                      7320, 7321, 7329, 6171, 6310, 6311, 8220,
                      5290, 5291, 7340, 5293, 5294, 5299, 1220,
                      6144, 4300, 7390, 1310, 9510, 5295, 9520,
                      9440, 7230, 9600, 9610, 9630, 7240, 3531,
                      9710, 9720, 3615, 9820, 5224, 5230, 8310,
                      4390, 8320, 7129, 8312, 8000, 7310, 7000,
                      8212, 6000, 5000, 7322, 8110, 8120, 7100,
                      7110, 7111, 7112, 7330, 7119, 7120, 7121, 
                      6100, 7130, 6110, 6111, 7140, 6119, 6120,
                      5100, 7150, 6312, 6130, 6131, 6132, 6142,
                      5110, 7160, 9210, 6140, 6141, 8190, 6143,
                      3330)

# childcare values
childcare_actlines = c(
  9230, # Travel escorting to/ from education
  9380, # Travel escorting a child (other than education)
  3800, # Unspecified childcare
  3810, # Unspecified physical care & supervision of a child
  3811, # Feeding the child
  3819, # Other and unspecified physical care & supervision of a child
  3820, # Teaching the child
  3830, # Reading, playing and talking with child
  3840, # Accompanying child
  3890) # Other specified childcare

leisure_actlines = c(personal_care_actlines, restrict_actlines)

################################################################################
################################## TABLE STUFF #################################
################################################################################

rows_within = c("y" = "Budget",
                "Bx_dev_wage_f_only" = "Budget * dev. fem. hourly pay",
                "Bx_dev_wage_m_only" = "Budget * dev. mal. hourly pay",
                "Bx_dev_educ_f_only" = "Budget * dev. fem. qualification",
                "Bx_dev_educ_m_only" = "Budget * dev. mal. qualification",
                "Bx_dev_avgage" = "Budget * dev. average age",
                "Bx_dev_agegap" = "Budget * dev. age gap")

rows_between = c("y" = "Budget",
                 "Bx_dev_wage_f_all" = "Budget * dev. all hourly pay",
                 "Bx_dev_wage_m_all" = "Budget * dev. all hourly pay",
                 "Bx_dev_educ_f_all" = "Budget * dev. all qualification",
                 "Bx_dev_educ_m_all" = "Budget * dev. all qualification",
                 "Bx_dev_avgage" = "Budget * dev. average age",
                 "Bx_dev_agegap" = "Budget * dev. age gap")

################################################################################
##### Male and female specifications: deviations from average with own sex #####
################################################################################

# leisure including eating, drinking, washing, spirituality, volunteering

# men
eq_m_within = private_leisure_exp_m ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# women
eq_f_within = private_leisure_exp_f ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# relaxing, socialising, sport, entertainment, and hobbies only

# men
eq_m_r_within = private_leisure_exp_r_m ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# women
eq_f_r_within = private_leisure_exp_r_f ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# list of regressions
eqns_within = list(male = eq_m_within, female = eq_f_within)
eqns_r_within = list(male = eq_m_r_within, female = eq_f_r_within)

################################################################################
### Male and female specifications: deviations from average of men AND women ###
################################################################################

# leisure including eating, drinking, washing, spirituality, volunteering

# men
eq_m_between = private_leisure_exp_m ~ 0 + y + Bx_dev_wage_f_all + Bx_dev_wage_m_all + 
  Bx_dev_educ_f_all + Bx_dev_educ_m_all + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# women
eq_f_between = private_leisure_exp_f ~ 0 + y + Bx_dev_wage_f_all + Bx_dev_wage_m_all + 
  Bx_dev_educ_f_all + Bx_dev_educ_m_all + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# relaxing, socialising, sport, entertainment, and hobbies only

# men
eq_m_r_between = private_leisure_exp_r_m ~ 0 + y + Bx_dev_wage_f_all + Bx_dev_wage_m_all + 
  Bx_dev_educ_f_all + Bx_dev_educ_m_all + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# women
eq_f_r_between = private_leisure_exp_r_f ~ 0 + y + Bx_dev_wage_f_all + Bx_dev_wage_m_all + 
  Bx_dev_educ_f_all + Bx_dev_educ_m_all + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# list of regressions
eqns_between = list(male = eq_m_between, female = eq_f_between)
eqns_r_between = list(male = eq_m_r_between, female = eq_f_r_between)

################################################################################
######### Male and female specifications: deviations from opposite sex #########
################################################################################

# leisure including eating, drinking, washing, spirituality, volunteering

# men
eq_m_opposite = private_leisure_exp_m ~ 0 + y + Bx_dev_wage_f_opp + Bx_dev_wage_m_opp + 
  Bx_dev_educ_f_opp + Bx_dev_educ_m_opp + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# women
eq_f_opposite = private_leisure_exp_f ~ 0 + y + Bx_dev_wage_f_opp + Bx_dev_wage_m_opp + 
  Bx_dev_educ_f_opp + Bx_dev_educ_m_opp + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# relaxing, socialising, sport, entertainment, and hobbies only

# men
eq_m_r_opposite = private_leisure_exp_r_m ~ 0 + y + Bx_dev_wage_f_opp + Bx_dev_wage_m_opp + 
  Bx_dev_educ_f_opp + Bx_dev_educ_m_opp + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# women
eq_f_r_opposite = private_leisure_exp_r_f ~ 0 + y + Bx_dev_wage_f_opp + Bx_dev_wage_m_opp + 
  Bx_dev_educ_f_opp + Bx_dev_educ_m_opp + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# list of regressions
eqns_opposite = list(male = eq_m_opposite, female = eq_f_opposite)
eqns_r_opposite = list(male = eq_m_r_opposite, female = eq_f_r_opposite)

################################################################################
######################## MATRIX OF RESTRICTIONS FOR SUR ########################
################################################################################

regs_within = c("Bx_dev_wage_f_only", "Bx_dev_wage_m_only",
                "Bx_dev_educ_f_only", "Bx_dev_educ_m_only",
                "Bx_dev_avgage", "Bx_dev_agegap", "Bx_dev_gdppc")

regs_between = c("Bx_dev_wage_f_all", "Bx_dev_wage_m_all",
                 "Bx_dev_educ_f_all", "Bx_dev_educ_m_all",
                 "Bx_dev_avgage", "Bx_dev_agegap", "Bx_dev_gdppc")

regs_opposite = c("Bx_dev_wage_f_opp", "Bx_dev_wage_m_opp",
                  "Bx_dev_educ_f_opp", "Bx_dev_educ_m_opp",
                  "Bx_dev_avgage", "Bx_dev_agegap", "Bx_dev_gdppc")

theta_names_single = c("theta_wf", "theta_wm", "theta_ef", "theta_em",
                       "theta_age", "theta_agegap", "theta_regwealth")

modReg_within = make_regMat(regressors = regs_within,
                            theta_names = theta_names_single)

modReg_between = make_regMat(regressors = regs_between,
                             theta_names = theta_names_single)

modReg_opposite = make_regMat(regressors = regs_opposite,
                              theta_names = theta_names_single)

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
