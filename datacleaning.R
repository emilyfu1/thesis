# import packages
library(ipumsr)
library(tidyverse)

setwd("/Users/emilyfu/Desktop/school/thesis")

# things to help with data cleaning
topcode_earnweek = 2884.61
invalid_earnweek = 99999.98
invalid_workhours = 995
invalid_race = 998
invalid_educ = 998
invalid_kid_dummy = 99
who_partner = c(200, 201)
who_private_exclude = c(200, 201, 202, 207, 300)
who_invalid = c(996, 997, 998)



childcare_actlines = c(030101, 030102, 030103, 030104, 030105, 030106, 030107, 
                       030108, 030109, 030110, 030111, 030112, 030199, 030201, 
                       030202, 030203, 030204, 030299, 030301, 030302, 030303, 
                       030399, 080101, 080102, 080199, 160107, 180301, 180302, 
                       180303, 180304, 180801)

leisure_actlines = c(010101, 010102, 010199, 010301, 010399, 010401, 010499, 
                     019999, 020603, 120201, 120202, 120299, 120301, 120302, 
                     120303, 120304, 120305, 120306, 120307, 120308, 120309, 
                     120310, 120311, 120312, 120313, 120399, 120401, 120402, 
                     120403, 120404, 120405, 120499, 120501, 120502, 120503, 
                     120504, 120599, 129999, 130101, 130102, 130103, 130104, 
                     130105, 130106, 130107, 130108, 130109, 130110, 130111, 
                     130112, 130113, 130114, 130115, 130116, 130117, 130118, 
                     130119, 130120, 130121, 130122, 130123, 130124, 130125, 
                     130126, 130127, 130128, 130129, 130130, 130131, 130132, 
                     130133, 130134, 130135, 130136, 130199, 130201, 130202, 
                     130203, 130204, 130205, 130206, 130207, 130208, 130209, 
                     130210, 130211, 130212, 130213, 130214, 130215, 130216, 
                     130217, 130218, 130219, 130220, 130221, 130222, 130223, 
                     130224, 130225, 130226, 130227, 130228, 130229, 130230, 
                     130231, 130232, 130299, 130301, 130302, 130399, 050201,
                     181201, 181202, 181203, 181204, 181205, 181206, 181299, 
                     181300, 181301, 181302)

# package download message
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

# define data extract DDI to use 
ATUS_ddi = 'atus_00001.xml'
# import ddi
ddi = read_ipums_ddi(ATUS_ddi)

# import data
data = read_ipums_micro(ddi) |>
  # strata isn't right after 2016 but it was preselected so im removing it
  # SERIAL makes CASEID redundant
  select(-STRATA, -CASEID)

# household level data
data_hh = data |>
  filter(RECTYPE == '1') |> # indicates a household observation) 
  
  # this will show all relevant varables collected at household level
  # it drops empty columns (i.e. data only collected for activities/individuals)
  select(where(~ !all(is.na(.)))) |>
  
  # drop rectype now in preparation for merge
  select(-RECTYPE)

# individual level data
data_individual = data |>
  filter(RECTYPE == '2') |> # indicates a person
  # this will show all relevant varables collected at individual level
  select(where(~ !all(is.na(.)))) |>
  
  # there isn't a straightforward way to find parents/couples...
  # but this can help identify kids and adults
  mutate(is_adult = AGE >= 18) |>
  
  # drop rectype now in preparation for merge
  select(-RECTYPE) |>
  
  # do an actual merge of household data and individual data together 
  # merge on YEAR, SERIAL
  inner_join(data_hh, by = c("YEAR", "SERIAL")) |>
  
  # find the respondent
  mutate(is_resp = LINENO == 1) |>
  
  # should probably generate an individual person identifier
  # this works since the ATUS is a repeated cross section
  mutate(person_id = row_number()) |>
  
  # populate SPOUSEPRES to the household level
  group_by("YEAR", "SERIAL") |>
  mutate(SPOUSEPRES = any(SPOUSEPRES == 1, na.rm = TRUE)) |>
  ungroup()

# who line data
data_who = data |>
  filter(RECTYPE == '4') |>
  select(where(~ !all(is.na(.)))) |>
  
  # drop rectype now in preparation for merge
  select(-RECTYPE) |>
  
  # to merge them together need to rename activity line number so its the same
  rename(ACTLINE = ACTLINEW)

# activities
data_activities = data |>
  filter(RECTYPE == '3') |>
  select(where(~ !all(is.na(.)))) |>
  # drop rectype now in preparation for merge
  select(-RECTYPE) |>
  
  # merging activities with information about who does them
  # merge on YEAR, SERIAL, ACTLINE for the who data
  inner_join(data_who, by = c("YEAR", "SERIAL", "ACTLINE")) |>
  
  # looks like WHOLINE in the who data aligns with LINENO in the individual data
  # and LINENOW in the who data is the person the activity was performed with 
  # confusing lmao
  # rename variables to make merging with individual level data possible
  rename(LINENO = WHOLINE, activity_done_with = LINENOW)

# separating types of hh members

# all adults 
data_adults = data_individual |>
  filter(is_adult) |>
  select(where(~ !all(is.na(.)))) |>
  
  # how many adults in each household?
  mutate(num_adults_in_HH = HH_SIZE - HH_NUMKIDS) |>
  
  # how many males and female adults in each household
  group_by(YEAR, SERIAL) |>
  mutate(num_male_adults = sum(SEX == 1 & AGE >= 18, na.rm=TRUE),
         num_female_adults = sum(SEX == 2 & AGE >= 18, na.rm=TRUE)) |>
  ungroup()

# all kids
data_kids = data_individual |>
  filter(!is_adult) |>
  select(where(~ !all(is.na(.))))

# individual data for hetero, working, cohabiting, parenting couples
data_working_parents = data_adults |>
  # keep two heterosexual parent households with children
  filter(HH_CHILD == 1, 
         SPOUSEPRES == 1, 
         num_adults_in_HH == 2,
         num_male_adults == 1,
         num_female_adults == 1) |>
  
  # populate incomes/working hours for spouses into the same variable
  # so i dont have to use SPEARNWEEK or SPUSUALHRS
  # first get household level helper variables
  group_by(YEAR, SERIAL) |>
  mutate(
    # respondent-reported spouse weekly earnings, fill to household level
    spouse_earnweek = if (any(SPEARNWEEK < invalid_earnweek, na.rm = TRUE)) {
      max(SPEARNWEEK[SPEARNWEEK < invalid_earnweek], na.rm = TRUE)
    } else NA_real_,
    
    # respondent-reported spouse usual hours, fill to household level
    spouse_usualhours = if (any(SPUSUALHRS < invalid_workhours, na.rm = TRUE)) {
      max(SPUSUALHRS[SPUSUALHRS < invalid_workhours], na.rm = TRUE)
    } else NA_real_,

    # fill in respondent-reported kid dummy variables to household level
    KID1TO2 = if (any(KID1TO2 < invalid_kid_dummy, na.rm = TRUE)) {
      max(KID1TO2[KID1TO2 < invalid_kid_dummy], na.rm = TRUE)
    } else NA_real_,
    KID3TO5 = if (any(KID3TO5 < invalid_kid_dummy, na.rm = TRUE)) {
      max(KID3TO5[KID3TO5 < invalid_kid_dummy], na.rm = TRUE)
    } else NA_real_,
    KID6TO12 = if (any(KID6TO12 < invalid_kid_dummy, na.rm = TRUE)) {
      max(KID6TO12[KID6TO12 < invalid_kid_dummy], na.rm = TRUE)
    } else NA_real_,
    KID13TO17 = if (any(KID13TO17 < invalid_kid_dummy, na.rm = TRUE)) {
      max(KID13TO17[KID13TO17 < invalid_kid_dummy], na.rm = TRUE)
    } else NA_real_
  ) |>
  ungroup() |>
  
  # use the helper variables to fill in
  mutate(EARNWEEK = if_else(is_resp, EARNWEEK, spouse_earnweek),
         UHRSWORKT = if_else(is_resp, UHRSWORKT, spouse_usualhours)) |>
  
  group_by(YEAR, SERIAL) |>
  mutate(
    # check for respondents who are working+earning regularly
    # validity for BOTH respondent and spouse
    valid_wage = (EARNWEEK < invalid_earnweek &
                         EARNWEEK > 0 &
                         UHRSWORKT > 0 &
                         UHRSWORKT < invalid_workhours)) |>
  # only keep respondents who are working+earning regularly
  filter(sum(valid_wage) == 2) |> 
  mutate(
    # earnings countributions of each person to household
    hh_total_earn = sum(EARNWEEK[EARNWEEK < invalid_earnweek], na.rm = TRUE),
    # how much is contributed
    earn_share = if_else(hh_total_earn > 0, EARNWEEK / hh_total_earn, NA_real_)) |>

  ungroup()

# merge individuals and their activities from the who/activity data
working_parents_act_who = data_working_parents |>
  inner_join(data_activities, by = c("YEAR", "SERIAL", "LINENO")) |>
  # make a variable to indicate if an activity is leisure
  mutate(activity_is_leisure = ACTIVITY %in% leisure_actlines) |>
  # make a variable to indicate if an activity is childcare
  mutate(activity_childcare = ACTIVITY %in% childcare_actlines) |>
  
  # make a variable to indicate if an activity is "private"
  mutate(activity_private = !(RELATEWU %in% who_private_exclude) & 
           !(RELATEWU %in% who_invalid)) |>
  # make a variable to indicate if an activity is done with a spouse/partner
  mutate(activity_withspouse = RELATEWU %in% who_partner) |>
  
  # private leisure
  mutate(private_leisure = activity_is_leisure & activity_private) |>
  # childcare without partner
  mutate(nospouse_childcare = activity_childcare & activity_private) |> 

  # individual-level variables for activity durations
  group_by(YEAR, SERIAL, person_id) |>
  summarise(total_leisure = sum(DURATION_EXT[activity_is_leisure], 
                                na.rm = TRUE),
            total_private_leisure = sum(DURATION_EXT[private_leisure], 
                                        na.rm = TRUE),
            total_childcare = sum(DURATION_EXT[activity_childcare], 
                                  na.rm = TRUE),
            total_childcare_nospouse = sum(DURATION_EXT[nospouse_childcare], 
                                           na.rm = TRUE),
            .groups = "drop") |>
  ungroup() |>
  
  # remove households if either spouse or respondent don't have private leisure
  group_by(YEAR, SERIAL) |>
  filter(all(total_private_leisure > 0),
         all(total_childcare > 0),
         all(total_childcare_private > 0)) |>
  ungroup() |>
  
  # somehow get back to individual level data? 

# only need YEAR, SERIAL, person_id, AGE, SEX, RACE, EDUCYRS, UHRSWORKT, EARNWEEK, hh_total_earn
# HH_NUMKIDS, total_leisure, total_private_leisure, total_childcare, total_childcare_nospouse

# save all the stuff
write.csv(data_hh,"atus_hh.csv", row.names = FALSE)
write.csv(data_individual,"atus_individuals.csv", row.names = FALSE)
write.csv(data_adults,"atus_adults.csv", row.names = FALSE)
write.csv(data_kids,"atus_kids.csv", row.names = FALSE)
write.csv(data_adults,"atus_adults.csv", row.names = FALSE)
write.csv(data_working_parents,"atus_working_parents_individual.csv", 
          row.names = FALSE)
write.csv(working_parents_act_who,"atus_working_parents_act.csv", 
          row.names = FALSE)