# import packages
library(ipumsr)
library(tidyverse)
library(haven)
wd = Sys.getenv("THESIS_WD")

setwd(wd)

atus_direct = paste0(wd, "/atus_data/")

# regional wealth (GDP per capita)
state_gdppc = read.csv(paste0(atus_direct, "regionalwealth_US.csv"))

# things to help with data cleaning
topcode_earnweek = 2884.61
invalid_earnweek = c(99999.99, 99999.98)
invalid_workhours = 995
invalid_race = 998
invalid_educ = 998
invalid_age = 85
invalid_sex = 99
invalid_kid_dummy = 99
who_partner = c(200, 201)
who_private_exclude = c(200, 201, 202, 207, 300)
who_invalid = c(996, 997, 998)

# childcare activities (currently used with childcare time summary variables)
childcare_actlines = c(030101, 030102, 030103, 030104, 030105, 030106, 030107, 
                       030108, 030109, 030110, 030111, 030112, 030199, 030201, 
                       030202, 030203, 030204, 030299, 030301, 030302, 030303, 
                       030399, 080101, 080102, 080199, 160103, 160107, 180301, 
                       180302, 180303, 180304, 180801)

# leisure including eating, drinking, washing, religious activity, volunteering
leisure_actlines = c(010101, 010102, 010199, 010301, 010399, 010401, 010499, 
                     019999, 050201, 020603, 120201, 120202, 120299, 120301,
                     120302, 120303, 120304, 120305, 120306, 120307, 120308,
                     120309, 120310, 120311, 120312, 120313, 120399, 120401, 
                     120402, 120403, 120404, 120405, 120499, 120501, 120502,
                     120503, 120504, 120599, 129999, 130101, 130102, 130103,
                     130104, 130105, 130106, 130107, 130108, 130109, 130110,
                     130111, 130112, 130113, 130114, 130115, 130116, 130117,
                     130118, 130119, 130120, 130121, 130122, 130123, 130124,
                     130125, 130126, 130127, 130128, 130129, 130130, 130131,
                     130132, 130133, 130134, 130135, 130136, 130199, 130201,
                     130202, 130203, 130204, 130205, 130206, 130207, 130208, 
                     130209, 130210, 130211, 130212, 130213, 130214, 130215,
                     130216, 130217, 130218, 130219, 130220, 130221, 130222,
                     130223, 130224, 130225, 130226, 130227, 130228, 130229,
                     130230, 130231, 130232, 130299, 130301, 130302, 130399,
                     140101, 140102, 140103, 140104, 140105, 149999, 150101,
                     150102, 150103, 150104, 150105, 150106, 150199, 150201,
                     150202, 150203, 150204, 150299, 150301, 150302, 150399,
                     150402, 150499, 150500, 150501, 150599, 150601, 150602,
                     150699, 150701, 150799, 150801, 150899, 159999, 180805,
                     181201, 181202, 181203, 181204, 181205, 181206, 181299,
                     181300, 181301, 181302, 181401, 181499, 181501, 181599,
                     010201, 010299, 060102, 060201, 060202, 060203, 060204,
                     060299, 060302, 060402, 070104, 110101, 110201, 110299, 
                     119999, 120101, 120199, 160101, 160102, 050202, 080501, 
                     080502, 080599, 181101, 181199)

# relaxing, socialising, sport, entertainment, and hobbies only
restrict_actlines = c(050201, 020603, 
                     120201, 120202, 120299, 120301, 120302, 120303, 120304, 
                     120305, 120306, 120307, 120308, 120309, 120310, 120311, 
                     120312, 120313, 120399, 120401, 120402, 120403, 120404, 
                     120405, 120499, 120501, 120502, 120503, 120504, 120599, 
                     129999, 130101, 130102, 130103, 130104, 130105, 130106, 
                     130107, 130108, 130109, 130110, 130111, 130112, 130113, 
                     130114, 130115, 130116, 130117, 130118, 130119, 130120, 
                     130121, 130122, 130123, 130124, 130125, 130126, 130127, 
                     130128, 130129, 130130, 130131, 130132, 130133, 130134, 
                     130135, 130136, 130199, 130201, 130202, 130203, 130204, 
                     130205, 130206, 130207, 130208, 130209, 130210, 130211, 
                     130212, 130213, 130214, 130215, 130216, 130217, 130218, 
                     130219, 130220, 130221, 130222, 130223, 130224, 130225, 
                     130226, 130227, 130228, 130229, 130230, 130231, 130232, 
                     130299, 130301, 130302, 130399, 140101, 140102, 140103, 
                     140104, 140105, 149999, 180805, 181201, 181202, 181203, 
                     181204, 181205, 181206, 181299, 181300, 181301, 181302, 
                     181401, 181499, 010299, 060102, 060201, 
                     060202, 060203, 060204, 060299, 060302, 060402, 070104, 
                     110101, 120101, 120199, 160101, 160102)

# package download message
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

# define data extract DDI to use 
ATUS_ddi = 'atus_00001.xml'
# import ddi
ddi = read_ipums_ddi(ATUS_ddi)

# import data
data = read_ipums_micro(ddi) |>
  # this is so that i can consider using the variable SCC_OWNHH_LN
  # for childcare instead of filtering ACTLINEs
  filter(YEAR >= 2004) |>
  # strata isn't right after 2016 but it was preselected so im removing it
  # SERIAL makes CASEID redundant
  select(-STRATA, -CASEID) |>
  # this removes labels (labels are still available in documentation)
  # gets rid of warnings due to slightly different phrasing
  zap_labels() 

# household level data (all)
data_hh = data |>
  filter(RECTYPE == '1') |> # indicates a household observation) 
  
  # this will show all relevant varables collected at household level
  # it drops empty columns (i.e. data only collected for activities/individuals)
  select(where(~ !all(is.na(.)))) |>
  
  # drop rectype now in preparation for merge
  select(-RECTYPE)

# individual level data (all)
data_individual = data |>
  filter(RECTYPE == '2') |> # indicates a person
  # keep only relevant variables
  select(where(~ !all(is.na(.)))) |>
  
  # this can help identify kids and adults
  mutate(is_adult = AGE >= 18) |>
  
  # drop rectype now in preparation for merge
  select(-RECTYPE) |>
  
  # do an actual merge of household data and individual data together 
  # merge on YEAR, SERIAL
  # this gets us household characteristics for the respondents
  inner_join(data_hh, by = c("YEAR", "SERIAL")) |>
  
  # add regional wealth
  # merge with GDP per capita
  inner_join(state_gdppc, by = c("YEAR", "STATEFIP")) |>
  
  # should generate an individual person identifier
  # this works since the ATUS is a repeated cross section
  mutate(person_id = row_number()) |>
  
  # keep valid race, age, sex
  filter(AGE <= invalid_age, 
         RACE < invalid_race,
         SEX < invalid_sex)

# who line data (all)
data_who = data |>
  filter(RECTYPE == '4') |> # indicates a person someone did activity with
  
  # keep only relevant variables
  select(where(~ !all(is.na(.)))) |>
  
  # drop rectype now in preparation for merge
  select(-RECTYPE) |>
  
  # to merge them together need to rename activity line number so its the same
  rename(ACTLINE = ACTLINEW)

# activities (all)
data_activities = data |>
  filter(RECTYPE == '3') |> # indicates an activity
  
  # keep only relevant variables
  select(where(~ !all(is.na(.)))) |>
  
  # drop rectype now in preparation for merge
  select(-RECTYPE) |>
  
  # merging activities with information about who does them
  # merge on YEAR, SERIAL, ACTLINE for the who data
  inner_join(data_who, by = c("YEAR", "SERIAL", "ACTLINE")) |>
  
  # looks like WHOLINE in the who data aligns with LINENO in the individual data
  rename(LINENO = LINENOW)

# separating types of hh members

# hetero, working, cohabiting, parents
# spouse characteristics are needed to calculate deviations
data_working_parents = data_individual |>
  
  # populate spouse and respondent variables to the household level
  # now these represent the characteristics of the household spouse/respondent
  group_by(YEAR, SERIAL) |>
  mutate(SPOUSEPRES = any(SPOUSEPRES == 1, na.rm = TRUE),
         
         # respondent-reported spouse characteristics
         # note that SPEDUC and EDUCYRS are categorised differently!
         # using mins here because the "invalid" labels are all larger
         SPAGE = min(SPAGE, na.rm=TRUE),
         SPSEX = min(SPSEX, na.rm=TRUE),
         SPRACE = min(SPRACE, na.rm=TRUE),
         SPEDUC = min(SPEDUC, na.rm=TRUE),
         SPUSUALHRS = min(SPUSUALHRS, na.rm=TRUE),
         SPEARNWEEK = min(SPEARNWEEK, na.rm=TRUE),
         
         # respondent-reported self characteristics
         # note that AGE, SEX, and RACE aren't respondent or spouse-specific
         RESPEDUCYRS = min(EDUCYRS, na.rm=TRUE),
         RESPUHRSWORKT = min(UHRSWORKT, na.rm=TRUE),
         RESPEARNWEEK = min(EARNWEEK, na.rm=TRUE),
         
         # finding respondents and spouses
         is_resp = LINENO == 1, # find the respondent
         # find the respondent person number
         resp_sploc = SPLOC[LINENO == 1][1],
         # find respondent's spouse using SPLOC
         is_spouse = LINENO == resp_sploc) |>
  
  ungroup() |>
  
  # finding valid and invalid spouses and respondents
  mutate(
         # valid regular earnings
         valid_earn_resp = !is.na(RESPEARNWEEK) &
           !(RESPEARNWEEK %in% invalid_earnweek) &
           RESPEARNWEEK > 0 & RESPEARNWEEK <= topcode_earnweek,
         
         valid_earn_spouse = !is.na(SPEARNWEEK) &
           !(SPEARNWEEK %in% invalid_earnweek) &
           SPEARNWEEK > 0 & SPEARNWEEK <= topcode_earnweek,
         
         # valid regular working hours
         valid_hours_resp = !is.na(RESPUHRSWORKT) & RESPUHRSWORKT > 0 &
           RESPUHRSWORKT < invalid_workhours,
         
         valid_hours_spouse = !is.na(SPUSUALHRS) & SPUSUALHRS > 0 &
           SPUSUALHRS < invalid_workhours,
         
         # valid education 
         valid_educ_resp = !is.na(RESPEDUCYRS) & RESPEDUCYRS < invalid_educ,
         valid_educ_spouse = !is.na(SPEDUC) & SPEDUC < invalid_educ,
         
         # define valid spouse and valid respondent
         valid_resp = valid_earn_resp & valid_hours_resp & valid_educ_resp,
         valid_spouse = valid_earn_spouse & valid_hours_spouse & 
           valid_educ_spouse) |>
  
  # keep only households where you truly observe a male-female couple 
  
  # get rid of entire households who don't have valid earnings, working hours,
  # education levels for both respondent and spouse
  group_by(YEAR, SERIAL) |>
  filter(sum(is_resp, na.rm = TRUE) == 1, # exactly one respondent
         sum(is_spouse, na.rm = TRUE) == 1, # exactly one spouse
         sum(is_resp | is_spouse, na.rm = TRUE) == 2,
         # one male and one female
         sum(SEX == 1 & (is_resp | is_spouse), na.rm = TRUE) == 1,
         sum(SEX == 2 & (is_resp | is_spouse), na.rm = TRUE) == 1) |>
  ungroup() |>
  
  # now keep only respondent + spouse rows
  filter(is_resp | is_spouse,
         is_adult,
         HH_CHILD == 1,
         SPOUSEPRES == 1,
         valid_resp,
         valid_spouse) |>
  
  # categorical education variable and populate earnings, working hours
  mutate(    
    # make SPEDUC and EDUCYRS equivalent
    resp_educ_cat = case_when(RESPEDUCYRS < 200 ~ 0, # below high school
                         # high school to some college
                         RESPEDUCYRS >= 200 & RESPEDUCYRS < 217 ~ 1,
                         # completed undergraduate/graduate degrees
                         RESPEDUCYRS >= 217 & RESPEDUCYRS <= 321 ~ 2,
                         TRUE ~ NA_real_),
    # spouse education in levels
    spouse_educ_cat = case_when(SPEDUC < 20 ~ 0, # below high school
                                # high school to some college 
                                # the spouse variable includes vocational
                                SPEDUC >= 20 & SPEDUC <= 32 ~ 1,
                                # completed undergraduate/graduate degrees
                                SPEDUC >= 40 & SPEDUC <= 43 ~ 2,
                                TRUE ~ NA_real_),
    
    # make EARNWEEK, UHRSWORKT, and educ_cat no longer respondent-specific
    EARNWEEK = if_else(is_resp, EARNWEEK, SPEARNWEEK),
    UHRSWORKT = if_else(is_resp, UHRSWORKT, SPUSUALHRS),
    educ_cat = if_else(is_resp, resp_educ_cat, spouse_educ_cat),
    
    # earnings contributions of each person to household
    hh_total_earn = RESPEARNWEEK + SPEARNWEEK,
    
    # how much is contributed by each member
    resp_earn_share = RESPEARNWEEK / hh_total_earn,
    spouse_earn_share = SPEARNWEEK / hh_total_earn,
    earn_share = EARNWEEK/hh_total_earn,
    
    # hourly wage of each member
    resp_hrly_wage = RESPEARNWEEK / UHRSWORKT,
    spouse_hrly_wage = SPEARNWEEK / SPUSUALHRS,
    hrly_wage = EARNWEEK / UHRSWORKT) |>
  
  # characteristics of the female and male couple of the household
  mutate(
    # within-couple sex specific wages
    wage_f = if_else(SEX == 2, resp_hrly_wage, spouse_hrly_wage),
    wage_m = if_else(SEX == 1, resp_hrly_wage, spouse_hrly_wage),
         
    # within-couple sex specific education
    educ_f = if_else(SEX == 2, resp_educ_cat, spouse_educ_cat),
    educ_m = if_else(SEX == 1, resp_educ_cat, spouse_educ_cat),
         
    # within-couple sex specific race
    race_f = if_else(SEX == 2, RACE, SPRACE),
    race_m = if_else(SEX == 1, RACE, SPRACE),
         
    # within-couple sex specific usual hours worked
    uhrsworkt_f = if_else(SEX == 2, UHRSWORKT, SPUSUALHRS),
    uhrsworkt_m = if_else(SEX == 1, UHRSWORKT, SPUSUALHRS),
         
    # within-couple sex specific age 
    age_f = if_else(SEX == 2, AGE, SPAGE),
    age_m = if_else(SEX == 1, AGE, SPAGE),
    
    # within-couple sex specific earnings contributions
    earn_share_f = if_else(SEX == 2, resp_earn_share, spouse_earn_share),
    earn_share_m = if_else(SEX == 1, resp_earn_share, spouse_earn_share),
    
    # average age of couple
    avgage = (age_f + age_m) / 2,
         
    # age gap (male spouse - female spouse)
    agegap_m = age_m - age_f)

# households in data_working_parents
hh_keys = data_working_parents |>
  distinct(YEAR, SERIAL)

# their children
data_kids = data_individual |>
  filter(!is_adult) |>
  inner_join(hh_keys, by = c("YEAR", "SERIAL"))

# how many kids of each sex
kids_counts = data_kids |>
  group_by(YEAR, SERIAL) |>
  summarise(num_kids_total = n(),
            num_kids_male = sum(SEX == 1, na.rm = TRUE),
            num_kids_female = sum(SEX == 2, na.rm = TRUE),
            .groups = "drop")

# kid age distribution
kids_age_dist = data_kids |>
  group_by(YEAR, SERIAL) |>
  summarise(kid_age_min  = min(AGE, na.rm = TRUE),
            kid_age_max  = max(AGE, na.rm = TRUE),
            kid_age_mean = mean(AGE, na.rm = TRUE),
                  
            n_kid_aged_0_2 = sum(AGE <= 2, na.rm = TRUE),
            n_kid_aged_3_5 = sum(AGE >= 3  & AGE <= 5, na.rm = TRUE),
            n_kid_aged_6_10 = sum(AGE >= 6  & AGE <= 10, na.rm = TRUE),
            n_kid_aged_11_13  = sum(AGE >= 11 & AGE <= 13, na.rm = TRUE),
            n_kid_aged_14_17 = sum(AGE >= 14 & AGE <= 17, na.rm = TRUE),
            .groups = "drop") |>
  select(YEAR, SERIAL, kid_age_min, kid_age_max, kid_age_mean, n_kid_aged_0_2,
         n_kid_aged_3_5, n_kid_aged_6_10, n_kid_aged_11_13, n_kid_aged_14_17)

# get ages of each kid
kids_age_wide = data_kids |>
  arrange(YEAR, SERIAL, desc(AGE)) |>
  group_by(YEAR, SERIAL) |>
  mutate(kid_index = row_number()) |>
  ungroup() |>
  select(YEAR, SERIAL, kid_index, AGE) |>
  pivot_wider(names_from = kid_index,
              values_from = AGE,
              names_prefix = "age_of_kid_")

# attach ages of kids to parents
data_working_parents = data_working_parents |>
  inner_join(kids_counts, by = c("YEAR", "SERIAL")) |>
  inner_join(kids_age_wide, by = c("YEAR", "SERIAL")) |>
  inner_join(kids_age_dist, by = c("YEAR", "SERIAL"))

# respondent-only rows (these are the only people with diaries)
resp_only = data_working_parents |>
  filter(is_resp)

# individual level time use for leisure and childcare activities
activity_summaries = resp_only |>
  inner_join(data_activities, by = c("YEAR", "SERIAL", "LINENO")) |>
  mutate(
    # types of activities
    activity_is_leisure = ACTIVITY %in% leisure_actlines,
    activity_is_leisure_r = ACTIVITY %in% restrict_actlines,
    activity_is_childcare = ACTIVITY %in% childcare_actlines,
    
    # private (no relevant household members present)
    # activities where "who" isn't asked are considered private
    activity_private = !(RELATEWU %in% who_private_exclude),
    # spouse not present
    activity_excludesspouse = !(RELATEWU %in% who_partner),
    
    # is private leisure?
    private_leisure = activity_is_leisure & activity_private,
    private_leisure_r = activity_is_leisure_r & activity_private) |>
  
  # find amount of minutes respondent spends for each activity category
  group_by(YEAR, SERIAL, person_id) |>
  summarise(
    total_leisure = sum(DURATION_EXT[activity_is_leisure], na.rm = TRUE),
    total_private_leisure = sum(DURATION_EXT[private_leisure], na.rm = TRUE),
    total_leisure_r = sum(DURATION_EXT[activity_is_leisure_r], na.rm = TRUE),
    total_private_leisure_r = sum(DURATION_EXT[private_leisure_r], na.rm = TRUE),
    
    total_childcare_act = sum(DURATION_EXT[activity_is_childcare], na.rm = TRUE),
    total_childcare_sec = sum(SCC_OWN_LN, na.rm = TRUE),
    total_childcare = max(total_childcare_act, total_childcare_sec),
    total_childcare_nospouse = sum(DURATION_EXT[activity_is_childcare & 
                                                  activity_excludesspouse], 
                                   na.rm = TRUE),
    .groups = "drop")


# check how many zeros are in data for leisure and childcare
valid_respondents = activity_summaries |>
  mutate(positive_private_leisure = total_private_leisure > 0,
         positive_private_leisure_r = total_private_leisure_r > 0) |>
  group_by(YEAR, SERIAL) |>
  summarise(
    n_diaries = n(), # one ATUS diary
    resp_zero_private_leisure = !any(positive_private_leisure, na.rm = TRUE),
    resp_zero_private_leisure_r = !any(positive_private_leisure_r, na.rm = TRUE),
    .groups = "drop") |>
  filter(n_diaries == 1)

sharing_est_data = resp_only |>
  inner_join(valid_respondents, by = c("YEAR", "SERIAL")) |>
  inner_join(activity_summaries, by = c("YEAR", "SERIAL", "person_id")) |>
  
  # expenditures
  mutate(
    # private leisure and childcare expenditure in hours
    total_private_leisure_h = total_private_leisure / 60,
    total_private_leisure_h_r = total_private_leisure_r / 60,
    total_childcare_nospouse_h = total_childcare_nospouse / 60,
    
    # respondent private leisure and childcare expenditure
    leisure_exp = hrly_wage * total_private_leisure_h,
    leisure_exp_r = hrly_wage * total_private_leisure_h_r,
    childcare_exp = hrly_wage * total_childcare_nospouse_h,
    
    # household budget
    y = (wage_f + wage_m) * 24,
    
    # observed private leisure expenditure for the respondent's gender only
    leisure_exp_m = if_else(SEX == 1, leisure_exp, NA_real_),
    leisure_exp_f = if_else(SEX == 2, leisure_exp, NA_real_),
    leisure_exp_m_r = if_else(SEX == 1, leisure_exp_r, NA_real_),
    leisure_exp_f_r = if_else(SEX == 2, leisure_exp_r, NA_real_),
    
    # observed private childcare expenditure for the respondent's gender only
    childcare_exp_m = if_else(SEX == 1, childcare_exp, NA_real_),
    childcare_exp_f = if_else(SEX == 2, childcare_exp, NA_real_)) |>
  
  # deviations from means of household-level characteristics
  group_by(YEAR) |>
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
    
    # deviations of average age of couple and age gap
    dev_avgage = avgage - mean(avgage, na.rm = TRUE),
    dev_agegap = agegap_m - mean(agegap_m, na.rm = TRUE),
    
    # regional wealth deviation
    dev_gdppc = gdp_pc_nominal - mean(gdp_pc_nominal, na.rm = TRUE)) |>
  
  ungroup() |>
  
  # interaction terms
  mutate(Bx_dev_wage_f_only = y * dev_wage_f_only,
         Bx_dev_wage_m_only = y * dev_wage_m_only,
         Bx_dev_educ_f_only = y * dev_educ_f_only,
         Bx_dev_educ_m_only = y * dev_educ_m_only,
    
         Bx_dev_wage_f_all = y * dev_wage_f_all,
         Bx_dev_wage_m_all = y * dev_wage_m_all,
         Bx_dev_educ_f_all = y * dev_educ_f_all,
         Bx_dev_educ_m_all = y * dev_educ_m_all,
    
         Bx_dev_avgage = y * dev_avgage,
         Bx_dev_agegap = y * dev_agegap,
         Bx_dev_gdppc = y * dev_gdppc)

males = sharing_est_data |> filter(SEX == 1)
females = sharing_est_data |> filter(SEX == 2)

# save all the stuff
write.csv(data_hh, paste0(atus_direct, "atus_hh.csv"), row.names = FALSE)
write.csv(data_individual, paste0(atus_direct, "atus_individuals.csv"), row.names = FALSE)
write.csv(data_kids, paste0(atus_direct, "atus_kids.csv"), row.names = FALSE)
write.csv(data_working_parents, paste0(atus_direct, "atus_working_parents_individual.csv"), 
          row.names = FALSE)
write.csv(sharing_est_data, paste0(atus_direct, "atus_working_parents_act.csv"), 
          row.names = FALSE)
write.csv(males, paste0(atus_direct, "atus_working_parents_act_males.csv"), row.names = FALSE)
write.csv(females, paste0(atus_direct, "atus_working_parents_act_females.csv"), row.names = FALSE)