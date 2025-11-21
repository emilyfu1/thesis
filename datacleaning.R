# import packages
library(ipumsr)
library(tidyverse)

setwd("/Users/emilyfu/Desktop/school/thesis")

topcode_earnweek = 2884.61
NIU_earnweek = 99999.99
NIU_workhours = 999
varying_workhours = 995
lookat = 2019

childcare_actlines = c(030101, 030102, 030103, 030104, 030105, 030106, 030107, 
                       030108, 030109, 030110, 030111, 030112, 030199, 030201, 
                       030202, 030203, 030204, 030299, 030301, 030302, 030303, 
                       030399, 080101, 080102, 080199, 160107, 180301, 180302, 
                       180303, 180304, 180801)

leisure_actlines = c(120201, 120202, 120299, 120301, 120302, 120303, 120304, 
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
                     130299, 130301, 130302, 130399)

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
  mutate(person_id = row_number())

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

# this is the main dataset that i'll be using
data_working_parents = data_adults |>
  # keep two heterosexual parent households with children
  filter(HH_CHILD == 1, 
         SPOUSEPRES == 1, 
         num_adults_in_HH == 2,
         num_male_adults == 1,
         num_female_adults == 1) |>
  # merge on YEAR, SERIAL, LINENO for the who data
  inner_join(data_activities, by = c("YEAR", "SERIAL", "LINENO"))

# generate is_partner variable to link partners (or use the same hh id)
# populate incomes/working hours for spouses
# aggregate working, leisure (alone), leisure (with household member)
# aggregate chilcare (without partner present)