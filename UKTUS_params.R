# setwd
wd = Sys.getenv("THESIS_WD")
setwd(wd)

# import functions
source("functions.R")

# regional wealth
source("regionalwealth.R")

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

vars_to_suffix = c(
  "wage", "educ", "NetWkly", "HrWkAc", "DVAge",
  "total_leisure", "total_leisure_r", "total_private_leisure",
  "total_private_leisure_r", "total_childcare", 
  "total_otherdomestic", "total_work", "total_sleep",
  "total_leisure_exp", "total_leisure_exp_r", "private_leisure_exp",
  "private_leisure_exp_r", "total_childcare_exp", "total_otherdomestic_exp",
  "y_individual", "pnum", "spouse_pnum")

################################################################################
################################ ACTIVITY CODES ################################
################################################################################

sleep_actlines = c(10, 110, 111)

personal_care_actlines = c(0, # Unspecified personal care
                           210, # Eating
                           1310, # Activities related to employment: Lunch break
                           5310, # Resting - time out
                           310, # Other personal care: Wash and dress
                           300, # Other personal care: Unspecified other personal care
                           390) # Other personal care: Other specified personal care

sleep_personalcare = c(sleep_actlines, personal_care_actlines)

# hobbies, social activities, entertainment
mainleisure_actlines = c(5120, 6149, 6150, 5292, 5130, 6160, 8210,
                      8211, 7190, 6170, 8219, 8221, 8222, 6179, 
                      8229, 6190, 5190, 7241, 5200, 7249, 7251,
                      5210, 1120, 5220, 5221, 5222, 5223, 5225,
                      8300, 5229, 8311, 5245, 8319, 5250, 7300,
                      7320, 7321, 7329, 6171, 6310, 6311, 8220,
                      5290, 5291, 7340, 5293, 5294, 5299, 1220,
                      6144, 4300, 7390, 5295, 7230, 7240, 3531,
                      3615, 9820, 5224, 5230, 8310, 4390, 8320, 
                      7129, 8312, 8000, 7310, 7000, 8212, 6000, 
                      5000, 7322, 8110, 8120, 7100, 7110, 7111, 
                      7112, 7330, 7119, 7120, 7121, 6100, 7130, 
                      6110, 6111, 7140, 6119, 6120, 5100, 7150, 
                      6312, 6130, 6131, 6132, 6142, 5110, 7160, 
                      6140, 6141, 8190, 6143, 3410, 8100)

travel_leisure_actlines = c(9500, # Travel to visit friends/relatives in their homes not respondents household
                            9620, # Travel related to hunting & fishing
                            9510, # Travel related to other social activities
                            9520, # Travel related to entertainment and culture
                            9600, # Travel related to other leisure
                            9610, # Travel related to physical exercise
                            9630, # Travel related to productive exercise other than hunting & fishing
                            9710, # Travel related to gambling
                            9720, # Travel related to hobbies other than gambling
                            9810, # Travel to holiday base
                            9820) # Travel for day trip/just walk

# leisure and borderline leisure activities not included in original paper
additional_leisure = c(7220, # Computing - programming
                       7250, # Unspecified other computing
                       7259, # Other specified computing
                       6200, # Unspecified productive exercise
                       7231, # Information searching on the internet
                       5241, # Borrowing books records audiotapes videotapes CDs VDs etc. from a library
                       5242, # Reference to books and other library materials within a library
                       5243, # Using internet in the library
                       5249, # Other specified library activities
                       6290, # Other specified productive exercise
                       2210, # Free time study
                       5240, # Unspecified library
                       5244) # Using computers in the library other than internet use

# activities that may be considered leisure and domestic/non-market work
borderline_leisure_domestic = c(3220, # Cleaning yard
                                3430, # Caring for pets
                                3330, # Handicraft and producing textiles
                                6210, # Hunting and fishing
                                3140, # Preserving
                                3390, # Other specified making and care for textiles
                                6220, # Picking berries mushroom and herbs
                                3410, # Gardening
                                3440, # Walking the dog
                                3490, # Other specified gardening and pet care
                                3614, # Shopping or browsing at car boot sales or antique fairs
                                3540) # Vehicle maintenance

# decide whether to include additional items
restrict_actlines = c(mainleisure_actlines)
leisure_actlines = c(sleep_actlines, personal_care_actlines,
                     restrict_actlines)

# working at job
work_actlines = c(
  9110, # Travel in the course of work
  1100, # Main job: unspecified main job
  1000, # Unspecified employment
  1110, # work at main job
  1210) # work at second job

# childcare values
childcare_actlines = c(
  9230, # Travel escorting to/ from education
  3800, # Unspecified childcare
  3810, # Unspecified physical care & supervision of a child
  3811, # Feeding the child
  3819, # Other and unspecified physical care & supervision of a child
  3820, # Teaching the child
  3830, # Reading, playing and talking with child
  3840, # Accompanying child
  3890, # Other specified childcare
  9230, # Travel escorting to/ from education
  9380) # Travel escorting a child (other than education)

# domestic work
otherdomestic_actlines = c(3100, # Unspecified food management
                           3110, # Food preparation and baking
                           3250, # Disposal of waste
                           3130, # Dish washing
                           6210, # Hunting and fishing
                           3140, # Preserving
                           3190, # Other specified food management
                           3200, # Unspecified household upkeep
                           3210, # Cleaning dwelling
                           3220, # Cleaning yard
                           3230, # Heating and water
                           3240, # Arranging household goods and materials
                           3290, # Other or unspecified household upkeep
                           3300, # Unspecified making and care for textiles
                           3310, # Laundry
                           3320, # Ironing
                           3330, # Handicraft and producing textiles
                           3390, # Other specified making and care for textiles
                           3420, # Tending domestic animals
                           3430, # Caring for pets
                           3490, # Other specified gardening and pet care
                           3500, # Unspecified construction and repairs
                           3510, # House construction and renovation
                           3520, # Repairs of dwelling
                           6220, # Picking berries mushroom and herbs
                           3530, # Making repairing and maintaining equipment
                           3539, # Other specified making repairing and maintaining equipment
                           3540, # Vehicle maintenance
                           3590, # Other specified construction and repairs
                           3600, # Unspecified shopping and services
                           3610, # Unspecified shopping
                           3611, # Shopping mainly for food
                           3612, # Shopping mainly for clothing
                           3613, # Shopping mainly related to accommodation
                           3614, # Shopping or browsing at car boot sales or antique fairs
                           3619, # Other specified shopping
                           4190, # Other specified organisational work
                           3000, # Unspecified household and family care
                           3690, # Other specified shopping and services
                           3710, # Household management not using the internet
                           3713, # Shopping for and ordering clothing via the internet
                           3720, # Unspecified household management using the internet
                           3721, # Shopping for and ordering unspecified goods and services via the internet
                           3722, # Shopping for and ordering food via the internet
                           3724, # Shopping for and ordering goods and services related to accommodation via the internet
                           3725, # Shopping for and ordering mass media via the internet
                           3726, # Shopping for and ordering entertainment via the internet
                           3727, # Banking and bill paying via the internet
                           3729, # Other specified household management using the internet
                           3910, # Unspecified help to a non-dependent eg injured adult household member
                           3911, # Physical care of a non-dependent e.g. injured adult household member
                           3914, # Accompanying a non-dependent adult household member e.g. to hospital
                           3919, # Other specified help to a non-dependent adult household member
                           3920, # Unspecified help to a dependent adult household member
                           3921, # Physical care of a dependent adult household member e.g. Alzheimic parent
                           3924, # Accompanying a dependent adult household member e.g. Alzheimic
                           3929) # Other specified help to a dependent adult household member

# all domestic work
domestic_actlines = c(childcare_actlines, otherdomestic_actlines)

################################################################################
############################# NONPARENT TABLE STUFF ############################
################################################################################

np_rows_within = c("y" = "Budget",
                "Bx_dev_wage_f_only" = "Budget * dev. fem. hourly pay",
                "Bx_dev_wage_m_only" = "Budget * dev. mal. hourly pay",
                "Bx_dev_educ_f_only" = "Budget * dev. fem. qualification",
                "Bx_dev_educ_m_only" = "Budget * dev. mal. qualification",
                "Bx_dev_avgage" = "Budget * dev. average age",
                "Bx_dev_agegap" = "Budget * dev. age gap",
                "Bx_dev_gdppc" = "Budget * dev. regional wealth p.c.")

################################################################################
############################### PARENT TABLE STUFF #############################
################################################################################

p_rows_within = c("y" = "Budget",
                  "Bx_dev_wage_f_only" = "Budget * dev. fem. hourly pay",
                  "Bx_dev_wage_m_only" = "Budget * dev. mal. hourly pay",
                  "Bx_dev_educ_f_only" = "Budget * dev. fem. qualification",
                  "Bx_dev_educ_m_only" = "Budget * dev. mal. qualification",
                  "Bx_dev_avgage" = "Budget * dev. average age",
                  "Bx_dev_agegap" = "Budget * dev. age gap",
                  "Bx_dev_gdppc" = "Budget * dev. regional wealth p.c.",
                  "Bx_dev_numkids" = "Budget * dev. num. kids",
                  "Bx_dev_numunder5" = "Budget * dev. num. kids under 5")

################################################################################
############################ Regressions non-parents ###########################
################################################################################

##### Male and female specifications: deviations from average with own sex #####

# leisure including sleep and personal care

# men
np_eq_m_within = private_leisure_exp_m ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# women
np_eq_f_within = private_leisure_exp_f ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# leisure excluding sleep and personal care

# men
np_eq_m_r_within = private_leisure_exp_r_m ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# women
np_eq_f_r_within = private_leisure_exp_r_f ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc

# list of regressions
np_eqns_within = list(male = np_eq_m_within, female = np_eq_f_within)
np_eqns_r_within = list(male = np_eq_m_r_within, female = np_eq_f_r_within)

################################################################################
############################## Regressions parents #############################
################################################################################

##### Male and female specifications: deviations from average with own sex #####

# leisure including sleep and personal care

# men
p_eq_m_within = private_leisure_exp_m ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc + 
  Bx_dev_numkids + Bx_dev_numunder5

# women
p_eq_f_within = private_leisure_exp_f ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc + 
  Bx_dev_numkids + Bx_dev_numunder5

# leisure excluding sleep and personal care

# men
p_eq_m_r_within = private_leisure_exp_r_m ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc + 
  Bx_dev_numkids + Bx_dev_numunder5

# women
p_eq_f_r_within = private_leisure_exp_r_f ~ 0 + y + Bx_dev_wage_f_only + Bx_dev_wage_m_only + 
  Bx_dev_educ_f_only + Bx_dev_educ_m_only + Bx_dev_avgage + Bx_dev_agegap + Bx_dev_gdppc + 
  Bx_dev_numkids + Bx_dev_numunder5

# list of regressions
p_eqns_within = list(male = p_eq_m_within, female = p_eq_f_within)
p_eqns_r_within = list(male = p_eq_m_r_within, female = p_eq_f_r_within)

######################## MATRIX OF RESTRICTIONS FOR SUR ########################

parents_addition = c("Bx_dev_numkids", "Bx_dev_numunder5")

np_regs_within = c("Bx_dev_wage_f_only", "Bx_dev_wage_m_only",
                "Bx_dev_educ_f_only", "Bx_dev_educ_m_only",
                "Bx_dev_avgage", "Bx_dev_agegap", "Bx_dev_gdppc")

p_regs_within = c(np_regs_within, parents_addition)

np_theta_names_single = c("theta_wf", "theta_wm", "theta_ef", "theta_em",
                       "theta_age", "theta_agegap", "theta_regwealth")

p_theta_names_single = c("theta_wf", "theta_wm", "theta_ef", "theta_em",
                          "theta_age", "theta_agegap", "theta_regwealth",
                         "theta_numkids", "theta_numunder5")

# restricted coefficients nonparents
np_modReg_within = make_regMat(regressors = np_regs_within,
                            theta_names = np_theta_names_single)

# restricted coefficients parents
p_modReg_within = make_regMat(regressors = p_regs_within,
                               theta_names = p_theta_names_single)


