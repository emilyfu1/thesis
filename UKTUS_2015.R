library(tidyverse)
library(haven)

# setwd
wd = Sys.getenv("THESIS_WD")
setwd(wd)

# actlines 
source("UKTUS_actlines.R")

# import data
data_2015_direct = "/UKDA-8128-stata/stata/stata11_se/"

# activity level data
data_activities = read_dta(paste0(wd, data_2015_direct, "uktus15_diary_ep_long.dta"))

# find which month to keep in the individual and household data
# only keep information collected at the same point as the time use data
diarymonth_households = data_activities |>
  distinct(serial, pnum, IMonth) |>
  arrange(serial, pnum)

# find time diaries and number of diaries everyone completes
individual_diaries = data_activities |>
  distinct(serial, pnum, daynum) |>
  group_by(serial, pnum) |>
  # household budget: i need to indicate how many days everyone has completed
  # so that i calculate expenditure and budget based on number of days
  mutate(num_diaries_filled = n())

# household data
data_hh = read_dta(paste0(wd, data_2015_direct, "uktus15_household.dta")) |>
  inner_join(diarymonth_households, by = c("serial", "IMonth"))

# individual level data
data_individual = read_dta(paste0(wd, data_2015_direct, "uktus15_individual.dta"))

# relationships in individual data
all_relationships = data_individual |> 
  pivot_longer(cols = starts_with("Relate"), 
               names_to = "relate_var", 
               values_to = "relation_to_pnum") |> 
  mutate(relevant_person = as.integer(gsub("Relate", "", relate_var))) |> 
  select(serial, pnum, relevant_person, relation_to_pnum) |>
  # filter down to each household size (filter out not applicables)
  filter(relation_to_pnum > 0) |>
  
  # dummy type of relationship
  mutate(pnum_is_spouse = relation_to_pnum == 1 | relation_to_pnum == 2 | relation_to_pnum == 3,
         pnum_is_child = relation_to_pnum == 4 | relation_to_pnum == 5 | relation_to_pnum == 6,
         pnum_is_parent = relation_to_pnum == 8 | relation_to_pnum == 9 | relation_to_pnum == 10)

# parents
data_working_parents = data_individual |>
  # keep only diary members and associated information
  inner_join(diarymonth_households, by = c("serial", "pnum", "IMonth")) |>
  # keep only hetero couples, child in household, both earning income
  filter(NumSSex == 0,
         NumCPart == 2 | NumMPart == 2 | NumCivP == 2,
         NumChild > 0, 
         NetWkly > 0,
         HrWkAc > 0) |>
  mutate(hrly_wage = NetWkly / HrWkAc, # calculated hourly wages
         is_resp = pnum == SelPer # find the respondent 
  )

# respective kids

# find spouse/partner pairs in data 
spouse_pairs = data_individual |> 
  pivot_longer(cols = starts_with("Relate"), 
               names_to = "relate_var", 
               values_to = "relate_code") |> 
  mutate(related_pnum = as.integer(gsub("Relate", "", relate_var))) |> 
  # filters for spouses, civil, and cohabiting partners
  filter(relate_code > 0, relate_code < 4) |> 
  select(serial, pnum, related_pnum) |> 
  # keep only pairs of spouses/partners
  group_by(serial) |>
  filter(n() == 2) |>
  ungroup() |>
  # rename as spouse_pnum 
  rename(spouse_pnum = related_pnum) |> 
  arrange(serial, pnum)

# finding time use
activity_summaries = data_activities |>
  select(serial, pnum, daynum, IMonth, IYear, eptime, whatdoing, What_Oth1,
         What_Oth2, What_Oth3, WithAlone, WithSpouse, WithChild, WithOther) |>
  
  # secondary activities and stuff
  mutate(
    activity1_is_leisure = whatdoing %in% leisure_actlines,
    activity1_is_leisure_r = whatdoing %in% restrict_actlines,
    activity1_is_childcare = whatdoing %in% childcare_actlines,
    activity2_is_leisure = What_Oth2 %in% leisure_actlines,
    activity2_is_leisure_r = What_Oth2 %in% restrict_actlines,
    activity2_is_childcare = What_Oth2 %in% childcare_actlines,
    activity3_is_leisure = What_Oth3 %in% leisure_actlines,
    activity3_is_leisure_r = What_Oth3 %in% restrict_actlines,
    activity3_is_childcare = What_Oth3 %in% childcare_actlines,
    
    # private (no relevant household members present)
    # activities where "who" isn't asked are considered private
    activity_private = WithSpouse == 0 & WithChild == 0,
    # spouse not present
    activity_excludesspouse = WithSpouse == 0,
    
    # general: is leisure?
    activity_is_leisure = (activity1_is_leisure | activity2_is_leisure | 
                             activity3_is_leisure),
    activity_is_leisure_r = (activity1_is_leisure_r | activity2_is_leisure_r | 
                             activity3_is_leisure_r),
    
    # general: is private leisure?
    private_leisure = activity_is_leisure & activity_private,
    private_leisure_r = activity_is_leisure_r & activity_private,
    
    # general: is childcare?
    activity_ischildcare = (activity1_is_childcare | activity2_is_childcare | 
                              activity3_is_childcare),

    # is no-spouse childcare?
    childcare_nospouse = activity_ischildcare & activity_excludesspouse) |>
  
  group_by(serial, pnum) |>
  summarise(
    total_leisure = sum(eptime[activity_is_leisure], na.rm = TRUE),
    total_private_leisure = sum(eptime[private_leisure], na.rm = TRUE),
    total_leisure_r = sum(eptime[activity1_is_leisure_r], na.rm = TRUE),
    total_private_leisure_r = sum(eptime[private_leisure_r], na.rm = TRUE),
    
    total_childcare = sum(eptime[activity_ischildcare], na.rm = TRUE),
    total_childcare_nospouse = sum(eptime[childcare_nospouse], na.rm = TRUE),
    .groups = "drop")

# merge everything together with individual data
sharing_est_data = data_individual |>
  # time use
  inner_join(activity_summaries, by = c("serial", "pnum")) |>
  # number of diaries
  inner_join(diarymonth_households, by = c("serial", "pnum")) |>
  

  
