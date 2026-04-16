library(tidyverse)
library(systemfit)

# setwd
wd = Sys.getenv("THESIS_WD")
setwd(wd)

# actlines and directories
source("UKTUS_params.R")

# functions
source("functions.R")

# data
source("datacleaning_UKTUS_2015.R")
source("datacleaning_UKTUS_2000.R")

parents_est_data_merged = bind_rows(
  parents_est_data_2000 |> mutate(sample = "2000"), 
  parents_est_data_2015 |> mutate(sample = "2015"))

parents_est_data_merged_weekday = bind_rows(
  parents_est_data_2000_weekday |> mutate(sample = "2000"), 
  parents_est_data_2015_weekday |> mutate(sample = "2015"))

parents_est_data_merged_weekend = bind_rows(
  parents_est_data_2000_weekend |> mutate(sample = "2000"), 
  parents_est_data_2015_weekend |> mutate(sample = "2015"))

nonparents_est_data_merged = bind_rows(
  nonparents_est_data_2000 |> mutate(sample = "2000"), 
  nonparents_est_data_2015 |> mutate(sample = "2015"))

nonparents_est_data_merged_weekday = bind_rows(
  nonparents_est_data_2000_weekday |> mutate(sample = "2000"), 
  nonparents_est_data_2015_weekday |> mutate(sample = "2015"))

nonparents_est_data_merged_weekend = bind_rows(
  nonparents_est_data_2000_weekend |> mutate(sample = "2000"), 
  nonparents_est_data_2015_weekend |> mutate(sample = "2015"))

everyone_est_data_merged = bind_rows(
  nonparents_est_data_merged_weekday |> mutate(is_parent = 0),
  parents_est_data_merged_weekday |> mutate(is_parent = 1))

everyone_est_data_2000 = everyone_est_data_merged |> filter(sample == "2000")
everyone_est_data_2015 = everyone_est_data_merged |> filter(sample == "2015")



# this is a super long block of code! work in progress to make it more efficient

################################################################################
#################################### PARENTS ###################################
################################################################################

############################### Using both years ###############################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
parents_unres_within_merged = systemfit(p_eqns_within, method = "SUR", 
                                    data = parents_est_data_merged_weekday)
# excluding sleep and personal care
parents_unres_r_within_merged = systemfit(p_eqns_r_within, method = "SUR", 
                                      data = parents_est_data_merged_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
parents_unres_between_merged = systemfit(p_eqns_between, method = "SUR", 
                                     data = parents_est_data_merged_weekday)
# excluding sleep and personal care
parents_unres_r_between_merged = systemfit(p_eqns_r_between, method = "SUR", 
                                       data = parents_est_data_merged_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
parents_unres_opposite_merged = systemfit(p_eqns_opposite, method = "SUR", 
                                      data = parents_est_data_merged_weekday)
# excluding sleep and personal care
parents_unres_r_opposite_merged = systemfit(p_eqns_r_opposite, method = "SUR", 
                                        data = parents_est_data_merged_weekday)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
parents_res_within_merged = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_merged_weekday,
                                  restrict.regMat = p_modReg_within)
# excluding sleep and personal care
parents_res_r_within_merged = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_merged_weekday, 
                                    restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
parents_res_between_merged = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_merged_weekday,
                                   restrict.regMat = p_modReg_between)
# excluding sleep and personal care
parents_res_r_between_merged = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_merged_weekday,
                                     restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
parents_res_opposite_merged = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_merged_weekday,
                                    restrict.regMat = p_modReg_opposite)
# excluding sleep and personal care
parents_res_r_opposite_merged = systemfit(p_eqns_r_opposite, method = "SUR", 
                                      data = parents_est_data_merged_weekday,
                                      restrict.regMat = p_modReg_opposite)

### calculate resource shares 

shares_parents_bothsex_merged = add_shares_from_lm(parents_res_between_merged, 
                                               data=parents_est_data_merged_weekday, 
                                               dev_type = "all",
                                               data_type = "parents")
shares_parents_bothsex_r_merged = add_shares_from_lm(parents_res_r_between_merged, 
                                                 data=parents_est_data_merged_weekday, 
                                                 dev_type = "all",
                                                 data_type = "parents")
shares_parents_ownsex_merged = add_shares_from_lm(parents_res_within_merged, 
                                              data=parents_est_data_merged_weekday, 
                                              dev_type = "own",
                                              data_type = "parents")
shares_parents_ownsex_r_merged = add_shares_from_lm(parents_res_r_within_merged, 
                                                data=parents_est_data_merged_weekday, 
                                                dev_type = "own",
                                                data_type = "parents")
shares_parents_oppsex_merged = add_shares_from_lm(parents_res_opposite_merged, 
                                              data=parents_est_data_merged_weekday, 
                                              dev_type = "opp",
                                              data_type = "parents")
shares_parents_oppsex_r_merged = add_shares_from_lm(parents_res_r_opposite_merged, 
                                                data=parents_est_data_merged_weekday, 
                                                dev_type = "opp",
                                                data_type = "parents")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
parents_unres_within_2015 = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_2015_weekday)
# excluding sleep and personal care
parents_unres_r_within_2015 = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_2015_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
parents_unres_between_2015 = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_2015_weekday)
# excluding sleep and personal care
parents_unres_r_between_2015 = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_2015_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
parents_unres_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_2015_weekday)
# excluding sleep and personal care
parents_unres_r_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                      data = parents_est_data_2015_weekday)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
parents_res_within_2015 = systemfit(p_eqns_within, method = "SUR", 
                                data = parents_est_data_2015_weekday,
                                restrict.regMat = p_modReg_within)
# excluding sleep and personal care
parents_res_r_within_2015 = systemfit(p_eqns_r_within, method = "SUR", 
                                  data = parents_est_data_2015_weekday, 
                                  restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
parents_res_between_2015 = systemfit(p_eqns_between, method = "SUR", 
                                 data = parents_est_data_2015_weekday,
                                 restrict.regMat = p_modReg_between)
# excluding sleep and personal care
parents_res_r_between_2015 = systemfit(p_eqns_r_between, method = "SUR", 
                                   data = parents_est_data_2015_weekday,
                                   restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
parents_res_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                  data = parents_est_data_2015_weekday,
                                  restrict.regMat = p_modReg_opposite)
# excluding sleep and personal care
parents_res_r_opposite_2015 = systemfit(p_eqns_r_opposite, method = "SUR", 
                                    data = parents_est_data_2015_weekday,
                                    restrict.regMat = p_modReg_opposite)

### calculate resource shares

shares_parents_bothsex_2015 = add_shares_from_lm(parents_res_between_2015, 
                                             data=parents_est_data_2015_weekday, 
                                             dev_type = "all",
                                             data_type = "parents")
shares_parents_bothsex_r_2015 = add_shares_from_lm(parents_res_r_between_2015, 
                                               data=parents_est_data_2015_weekday, 
                                               dev_type = "all",
                                               data_type = "parents")
shares_parents_ownsex_2015 = add_shares_from_lm(parents_res_within_2015, 
                                            data=parents_est_data_2015_weekday, 
                                            dev_type = "own",
                                            data_type = "parents")
shares_parents_ownsex_r_2015 = add_shares_from_lm(parents_res_r_within_2015, 
                                              data=parents_est_data_2015_weekday, 
                                              dev_type = "own",
                                              data_type = "parents")
shares_parents_oppsex_2015 = add_shares_from_lm(parents_res_opposite_2015, 
                                            data=parents_est_data_2015_weekday, 
                                            dev_type = "opp",
                                            data_type = "parents")
shares_parents_oppsex_r_2015 = add_shares_from_lm(parents_res_r_opposite_2015, 
                                              data=parents_est_data_2015_weekday, 
                                              dev_type = "opp",
                                              data_type = "parents")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
parents_unres_within_2000 = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_2000_weekday)
# excluding sleep and personal care
parents_unres_r_within_2000 = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_2000_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
parents_unres_between_2000 = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_2000_weekday)
# excluding sleep and personal care
parents_unres_r_between_2000 = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_2000_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
parents_unres_opposite_2000 = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_2000_weekday)
# excluding sleep and personal care
parents_unres_r_opposite_2000 = systemfit(p_eqns_r_opposite, method = "SUR", 
                                      data = parents_est_data_2000_weekday)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
parents_res_within_2000 = systemfit(p_eqns_within, method = "SUR", 
                                data = parents_est_data_2000_weekday,
                                restrict.regMat = p_modReg_within)
# excluding sleep and personal care
parents_res_r_within_2000 = systemfit(p_eqns_r_within, method = "SUR", 
                                  data = parents_est_data_2000_weekday, 
                                  restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
parents_res_between_2000 = systemfit(p_eqns_between, method = "SUR", 
                                 data = parents_est_data_2000_weekday,
                                 restrict.regMat = p_modReg_between)
# excluding sleep and personal care
parents_res_r_between_2000 = systemfit(p_eqns_r_between, method = "SUR", 
                                   data = parents_est_data_2000_weekday,
                                   restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
parents_res_opposite_2000 = systemfit(p_eqns_opposite, method = "SUR", 
                                  data = parents_est_data_2000_weekday,
                                  restrict.regMat = p_modReg_opposite)
# excluding sleep and personal care
parents_res_r_opposite_2000 = systemfit(p_eqns_r_opposite, method = "SUR", 
                                    data = parents_est_data_2000_weekday,
                                    restrict.regMat = p_modReg_opposite)

### calculate resource shares

shares_parents_bothsex_2000 = add_shares_from_lm(parents_res_between_2000, 
                                             data=parents_est_data_2000_weekday, 
                                             dev_type = "all",
                                             data_type = "parents")
shares_parents_bothsex_r_2000 = add_shares_from_lm(parents_res_r_between_2000, 
                                               data=parents_est_data_2000_weekday, 
                                               dev_type = "all",
                                               data_type = "parents")
shares_parents_ownsex_2000 = add_shares_from_lm(parents_res_within_2000, 
                                            data=parents_est_data_2000_weekday, 
                                            dev_type = "own",
                                            data_type = "parents")
shares_parents_ownsex_r_2000 = add_shares_from_lm(parents_res_r_within_2000, 
                                              data=parents_est_data_2000_weekday, 
                                              dev_type = "own",
                                              data_type = "parents")
shares_parents_oppsex_2000 = add_shares_from_lm(parents_res_opposite_2000, 
                                            data=parents_est_data_2000_weekday, 
                                            dev_type = "opp",
                                            data_type = "parents")
shares_parents_oppsex_r_2000 = add_shares_from_lm(parents_res_r_opposite_2000, 
                                              data=parents_est_data_2000_weekday, 
                                              dev_type = "opp",
                                              data_type = "parents")

############################ Counterfactual analysis ###########################

# including sleep and personal care

# the 2000 sharing rule using the 2015 data
counterfactual_2000sharing_2015data = add_shares_from_lm(
  parents_res_within_2000, data = parents_est_data_2015_weekday,
  dev_type = "own", data_type = "parents")

# the 2015 sharing rule using the 2000 data
counterfactual_2015sharing_2000data = add_shares_from_lm(
  parents_res_within_2015, data = parents_est_data_2000_weekday,
  dev_type = "own", data_type = "parents")

# the main sharing rule (estimated using both years) using the 2000 data
counterfactual_mainsharing_2000data = add_shares_from_lm(
  parents_res_within_merged, data=parents_est_data_2000_weekday, dev_type = "own",
  data_type = "parents")

# the main sharing rule (estimated using both years) using the 2015 data
counterfactual_mainsharing_2015data = add_shares_from_lm(
  parents_res_within_merged, data=parents_est_data_2015_weekday, dev_type = "own",
  data_type = "parents")

# the 2000 sharing rule using all data
counterfactual_2000sharing_alldata = add_shares_from_lm(
  parents_res_within_2000, data=parents_est_data_merged_weekday, dev_type = "own",
  data_type = "parents")

# the 2015 sharing rule using all data
counterfactual_2015sharing_alldata = add_shares_from_lm(
  parents_res_within_2015, data=parents_est_data_merged_weekday, dev_type = "own",
  data_type = "parents")

# excluding sleep and personal care

# the 2000 sharing rule using the 2015 data
counterfactual_2000sharing_2015data_r = add_shares_from_lm(
  parents_res_r_within_2000, data = parents_est_data_2015_weekday,
  dev_type = "own", data_type = "parents")

# the 2015 sharing rule using the 2000 data
counterfactual_2015sharing_2000data_r = add_shares_from_lm(
  parents_res_r_within_2015, data = parents_est_data_2000_weekday,
  dev_type = "own", data_type = "parents")

# the main sharing rule (estimated using both years) using the 2000 data
counterfactual_mainsharing_2000data_r = add_shares_from_lm(
  parents_res_r_within_merged, data=parents_est_data_2000_weekday, dev_type = "own",
  data_type = "parents")

# the main sharing rule (estimated using both years) using the 2015 data
counterfactual_mainsharing_2015data_r = add_shares_from_lm(
  parents_res_r_within_merged, data=parents_est_data_2015_weekday, dev_type = "own",
  data_type = "parents")

# the 2000 sharing rule using all data
counterfactual_2000sharing_alldata_r = add_shares_from_lm(
  parents_res_r_within_2000, data=parents_est_data_merged_weekday, dev_type = "own",
  data_type = "parents")

# the 2015 sharing rule using all data
counterfactual_2015sharing_alldata_r = add_shares_from_lm(
  parents_res_r_within_2015, data=parents_est_data_merged_weekday, dev_type = "own",
  data_type = "parents")

########################## merge with sharing_est_data #########################

parents_est_data_merged_shares = inner_join(shares_parents_ownsex_r_merged$data,
                                            parents_est_data_merged,
                                            by=c('serial')) |>
  rename(shareown_etahat_r_f = shareown_etahat_f,
         shareown_etahat_r_m = shareown_etahat_m) |>
  inner_join(shares_parents_ownsex_merged$data, by=c('serial')) |>
  mutate(share_budget_leisure_r = (private_leisure_exp_r_f + 
                                     private_leisure_exp_r_m) / y,
         share_budget_leisure = (private_leisure_exp_f + 
                                   private_leisure_exp_m) / y)

# weekday to weekend changes
delta_data = parents_est_data_merged_shares |>
  select(serial, sample, is_weekend, dgorpaf, rgdppc,                                                                                                                                                              
         total_childcare_m, total_childcare_f,                                                                                                                                                                     
         total_private_leisure_m, total_private_leisure_f,
         total_private_leisure_r_m, total_private_leisure_r_f,
         total_work_m, total_work_f,                                                                                                                                                                               
         total_otherdomestic_m, total_otherdomestic_f,
         shareown_etahat_f, shareown_etahat_m,
         shareown_etahat_r_f, shareown_etahat_r_m,
         num_kids_total, kid_age_min, num0_2, 
         num3_4, num5_9, num10_15, num16_17, 
         avgage, agegap_m,
         child_under_five, num_under_5, num_under_10, 
         has_young_child, has_childcare_help, has_otherdomestic_help) |>
  pivot_wider(names_from = is_weekend,
              values_from = c(starts_with("total_")),
              names_sep = "_") |>
  # weekend - weekday
  mutate(delta_total_childcare_m = total_childcare_m_0 - total_childcare_m_1,
         delta_total_childcare_f = total_childcare_f_0 - total_childcare_f_1,
         delta_total_private_leisure_m = total_private_leisure_m_0 - total_private_leisure_m_1,
         delta_total_private_leisure_f = total_private_leisure_f_0 - total_private_leisure_f_1,
         delta_total_private_leisure_r_m = total_private_leisure_r_m_0 - total_private_leisure_r_m_1,
         delta_total_private_leisure_r_f = total_private_leisure_r_f_0 - total_private_leisure_r_f_1,
         delta_total_work_m = total_work_m_0 - total_work_m_1,
         delta_total_work_f = total_work_f_0 - total_work_f_1,
         delta_total_otherdomestic_m = total_otherdomestic_m_0 - total_otherdomestic_m_1,
         delta_total_otherdomestic_f = total_otherdomestic_f_0 - total_otherdomestic_f_1) |>
  mutate(delta_cc_hh = delta_total_childcare_m + delta_total_childcare_f)

###################### merge with individual/day level data ####################

# individual-day level data
reg_stacked = bind_rows(
  data_working_parents_2000 |>
    transmute(
      serial, sample = "2000", dgorpaf, pnum, is_weekend, male,
      has_childcare_help, has_otherdomestic_help, wage, 
      total_work, total_otherdomestic, total_private_leisure,
      total_private_leisure_r, total_childcare, private_leisure_exp, 
      total_childcare_exp, total_otherdomestic_exp, private_leisure_exp_r,
      num_kids_total, kid_age_min, num0_2, num3_4, num5_9, num10_15, num16_17,
      child_under_five, num_under_5, num_under_10, has_young_child),
  data_working_parents_2015 |>
    transmute(
      serial, sample = "2015", dgorpaf, pnum, is_weekend, male,
      has_childcare_help, has_otherdomestic_help, wage, 
      total_work, total_otherdomestic, total_private_leisure,
      total_private_leisure_r, total_childcare, private_leisure_exp, 
      total_childcare_exp, total_otherdomestic_exp, private_leisure_exp_r,
      num_kids_total, kid_age_min, num0_2, num3_4, num5_9, num10_15, num16_17,
      child_under_five, num_under_5, num_under_10, has_young_child)) |>
  # time dummies
  mutate(dummy_2000 = ifelse(sample == "2000", 1, 0),
         dummy_2015 = ifelse(sample == "2015", 1, 0)) |>
  
  # merge with resource share estimates
  inner_join(parents_est_data_merged_shares |> select(serial, dgorpaf, 
                                                      is_weekend, wage_f, 
                                                      wage_m, avgage, agegap_m,
                                                      shareown_etahat_f,
                                                      shareown_etahat_m,
                                                      shareown_etahat_r_f,
                                                      shareown_etahat_r_m), 
             by=c("serial", "dgorpaf", "is_weekend")) |>
  
  mutate(over_75p_fem_share = if_else(shareown_etahat_f > quantile(shareown_etahat_f, 0.75), 1, 0),
         over_75p_fem_share_r = if_else(shareown_etahat_r_f > quantile(shareown_etahat_f, 0.75), 1, 0)) |>
  group_by(sample, serial, pnum) |>
  mutate(individual_id = row_number()) |>
  ungroup()

# all time use
timeuse_long = reg_stacked |>
  transmute(
    shareown_etahat_f,
    shareown_etahat_r_f,
    day = if_else(is_weekend == 1, "Weekend", "Weekday"),
    sex = if_else(male == 1, "Men", "Women"),
    total_private_leisure,
    total_private_leisure_r,
    total_childcare,
    total_work,
    total_otherdomestic) |>
  pivot_longer(
    cols = c(total_private_leisure, total_private_leisure_r, 
             total_childcare, total_work, total_otherdomestic),
    names_to = "activity",
    values_to = "hours") |>
  mutate(
    activity = factor(
      activity,
      levels = c("total_private_leisure", 
                 "total_private_leisure_r", "total_childcare", "total_work", 
                 "total_otherdomestic"),
      labels = c("Private leisure",
                 "Private leisure (excluding sleep and personal care)",
                 "Childcare",
                 "Market work",
                 "Domestic work excluding childcare")),
    share_quartile = ntile(shareown_etahat_f, 4),
    share_quartile = factor(
      share_quartile,
      levels = 1:4,
      labels = c("p0–25", "p25–50", "p50–75", "p75–100")),
    share_quartile_r = ntile(shareown_etahat_r_f, 4),
    share_quartile_r = factor(
      share_quartile_r,
      levels = 1:4,
      labels = c("p0–25", "p25–50", "p50–75", "p75–100")))

################################################################################
################################## NON-PARENTS #################################
################################################################################

############################### Using both years ###############################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
nonparents_unres_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                    data = nonparents_est_data_merged_weekday)
# excluding sleep and personal care
nonparents_unres_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                      data = nonparents_est_data_merged_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
nonparents_unres_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                     data = nonparents_est_data_merged_weekday)
# excluding sleep and personal care
nonparents_unres_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                       data = nonparents_est_data_merged_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
nonparents_unres_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_merged_weekday)
# excluding sleep and personal care
nonparents_unres_r_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                        data = nonparents_est_data_merged_weekday)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
nonparents_res_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_merged_weekday,
                                  restrict.regMat = np_modReg_within)
# excluding sleep and personal care
nonparents_res_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_merged_weekday, 
                                    restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
nonparents_res_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_merged_weekday,
                                   restrict.regMat = np_modReg_between)
# excluding sleep and personal care
nonparents_res_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_merged_weekday,
                                     restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
nonparents_res_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_merged_weekday,
                                    restrict.regMat = np_modReg_opposite)
# excluding sleep and personal care
nonparents_res_r_opposite_merged = systemfit(np_eqns_r_opposite, method = "SUR", 
                                      data = nonparents_est_data_merged_weekday,
                                      restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_nonparents_bothsex_merged = add_shares_from_lm(nonparents_res_between_merged, 
                                               data=nonparents_est_data_merged_weekday, 
                                               dev_type = "all",
                                               data_type = "nonparents")
shares_nonparents_bothsex_r_merged = add_shares_from_lm(nonparents_res_r_between_merged, 
                                                 data=nonparents_est_data_merged_weekday, 
                                                 dev_type = "all",
                                                 data_type = "nonparents")
shares_nonparents_ownsex_merged = add_shares_from_lm(nonparents_res_within_merged, 
                                              data=nonparents_est_data_merged_weekday, 
                                              dev_type = "own",
                                              data_type = "nonparents")
shares_nonparents_ownsex_r_merged = add_shares_from_lm(nonparents_res_r_within_merged, 
                                                data=nonparents_est_data_merged_weekday, 
                                                dev_type = "own",
                                                data_type = "nonparents")
shares_nonparents_oppsex_merged = add_shares_from_lm(nonparents_res_opposite_merged, 
                                              data=nonparents_est_data_merged_weekday, 
                                              dev_type = "opp",
                                              data_type = "nonparents")
shares_nonparents_oppsex_r_merged = add_shares_from_lm(nonparents_res_r_opposite_merged, 
                                                data=nonparents_est_data_merged_weekday, 
                                                dev_type = "opp",
                                                data_type = "nonparents")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
nonparents_unres_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_2015_weekday)
# excluding sleep and personal care
nonparents_unres_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_2015_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
nonparents_unres_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_2015_weekday)
# excluding sleep and personal care
nonparents_unres_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_2015_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
nonparents_unres_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_2015_weekday)
# excluding sleep and personal care
nonparents_unres_r_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_2015_weekday)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
nonparents_res_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                data = nonparents_est_data_2015_weekday,
                                restrict.regMat = np_modReg_within)
# excluding sleep and personal care
nonparents_res_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                  data = nonparents_est_data_2015_weekday, 
                                  restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
nonparents_res_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                 data = nonparents_est_data_2015_weekday,
                                 restrict.regMat = np_modReg_between)
# excluding sleep and personal care
nonparents_res_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                   data = nonparents_est_data_2015_weekday,
                                   restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
nonparents_res_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                  data = nonparents_est_data_2015_weekday,
                                  restrict.regMat = np_modReg_opposite)
# excluding sleep and personal care
nonparents_res_r_opposite_2015 = systemfit(np_eqns_r_opposite, method = "SUR", 
                                    data = nonparents_est_data_2015_weekday,
                                    restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_nonparents_bothsex_2015 = add_shares_from_lm(nonparents_res_between_2015, 
                                             data=nonparents_est_data_2015_weekday, 
                                             dev_type = "all",
                                             data_type = "nonparents")
shares_nonparents_bothsex_r_2015 = add_shares_from_lm(nonparents_res_r_between_2015, 
                                               data=nonparents_est_data_2015_weekday, 
                                               dev_type = "all",
                                               data_type = "nonparents")
shares_nonparents_ownsex_2015 = add_shares_from_lm(nonparents_res_within_2015, 
                                            data=nonparents_est_data_2015_weekday, 
                                            dev_type = "own",
                                            data_type = "nonparents")
shares_nonparents_ownsex_r_2015 = add_shares_from_lm(nonparents_res_r_within_2015, 
                                              data=nonparents_est_data_2015_weekday, 
                                              dev_type = "own",
                                              data_type = "nonparents")
shares_nonparents_oppsex_2015 = add_shares_from_lm(nonparents_res_opposite_2015, 
                                            data=nonparents_est_data_2015_weekday, 
                                            dev_type = "opp",
                                            data_type = "nonparents")
shares_nonparents_oppsex_r_2015 = add_shares_from_lm(nonparents_res_r_opposite_2015, 
                                              data=nonparents_est_data_2015_weekday, 
                                              dev_type = "opp",
                                              data_type = "nonparents")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
nonparents_unres_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_2000_weekday)
# excluding sleep and personal care
nonparents_unres_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_2000_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
nonparents_unres_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_2000_weekday)
# excluding sleep and personal care
nonparents_unres_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_2000_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
nonparents_unres_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_2000_weekday)
# excluding sleep and personal care
nonparents_unres_r_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_2000_weekday)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
nonparents_res_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                data = nonparents_est_data_2000_weekday,
                                restrict.regMat = np_modReg_within)
# excluding sleep and personal care
nonparents_res_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                  data = nonparents_est_data_2000_weekday, 
                                  restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
nonparents_res_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                 data = nonparents_est_data_2000_weekday,
                                 restrict.regMat = np_modReg_between)
# excluding sleep and personal care
nonparents_res_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                   data = nonparents_est_data_2000_weekday,
                                   restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
nonparents_res_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                  data = nonparents_est_data_2000_weekday,
                                  restrict.regMat = np_modReg_opposite)
# excluding sleep and personal care
nonparents_res_r_opposite_2000 = systemfit(np_eqns_r_opposite, method = "SUR", 
                                    data = nonparents_est_data_2000_weekday,
                                    restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_nonparents_bothsex_2000 = add_shares_from_lm(nonparents_res_between_2000, 
                                             data=nonparents_est_data_2000_weekday, 
                                             dev_type = "all",
                                             data_type = "nonparents")
shares_nonparents_bothsex_r_2000 = add_shares_from_lm(nonparents_res_r_between_2000, 
                                               data=nonparents_est_data_2000_weekday, 
                                               dev_type = "all",
                                               data_type = "nonparents")
shares_nonparents_ownsex_2000 = add_shares_from_lm(nonparents_res_within_2000, 
                                            data=nonparents_est_data_2000_weekday, 
                                            dev_type = "own",
                                            data_type = "nonparents")
shares_nonparents_ownsex_r_2000 = add_shares_from_lm(nonparents_res_r_within_2000, 
                                              data=nonparents_est_data_2000_weekday, 
                                              dev_type = "own",
                                              data_type = "nonparents")
shares_nonparents_oppsex_2000 = add_shares_from_lm(nonparents_res_opposite_2000, 
                                            data=nonparents_est_data_2000_weekday, 
                                            dev_type = "opp",
                                            data_type = "nonparents")
shares_nonparents_oppsex_r_2000 = add_shares_from_lm(nonparents_res_r_opposite_2000, 
                                              data=nonparents_est_data_2000_weekday, 
                                              dev_type = "opp",
                                              data_type = "nonparents")

########################## merge with sharing_est_data #########################

nonparents_est_data_merged_shares = inner_join(shares_nonparents_ownsex_r_merged$data,
                                               nonparents_est_data_merged_weekday,
                                               by=c('serial')) |>
  rename(shareown_etahat_r_f = shareown_etahat_f,
         shareown_etahat_r_m = shareown_etahat_m) |>
  inner_join(shares_nonparents_ownsex_merged$data, by=c('serial')) |>
  mutate(share_budget_leisure_r = (private_leisure_exp_r_f + 
                                     private_leisure_exp_r_m) / y,
         share_budget_leisure = (private_leisure_exp_f + 
                                   private_leisure_exp_m) / y)

################################################################################
###################################### BOTH ####################################
################################################################################

############################### Using both years ###############################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
everyone_unres_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                           data = everyone_est_data_merged)
# excluding sleep and personal care
everyone_unres_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                             data = everyone_est_data_merged)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
everyone_unres_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                            data = everyone_est_data_merged)
# excluding sleep and personal care
everyone_unres_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                              data = everyone_est_data_merged)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
everyone_unres_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                             data = everyone_est_data_merged)
# excluding sleep and personal care
everyone_unres_r_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                               data = everyone_est_data_merged)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
everyone_res_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                         data = everyone_est_data_merged,
                                         restrict.regMat = np_modReg_within)
# excluding sleep and personal care
everyone_res_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = everyone_est_data_merged, 
                                           restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
everyone_res_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                          data = everyone_est_data_merged,
                                          restrict.regMat = np_modReg_between)
# excluding sleep and personal care
everyone_res_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                            data = everyone_est_data_merged,
                                            restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
everyone_res_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                           data = everyone_est_data_merged,
                                           restrict.regMat = np_modReg_opposite)
# excluding sleep and personal care
everyone_res_r_opposite_merged = systemfit(np_eqns_r_opposite, method = "SUR", 
                                             data = everyone_est_data_merged,
                                             restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_everyone_bothsex_merged = add_shares_from_lm(everyone_res_between_merged, 
                                                      data=everyone_est_data_merged, 
                                                      dev_type = "all",
                                                    data_type = "nonparents")
shares_everyone_bothsex_r_merged = add_shares_from_lm(everyone_res_r_between_merged, 
                                                        data=everyone_est_data_merged, 
                                                        dev_type = "all",
                                                      data_type = "nonparents")
shares_everyone_ownsex_merged = add_shares_from_lm(everyone_res_within_merged, 
                                                     data=everyone_est_data_merged, 
                                                     dev_type = "own",
                                                   data_type = "nonparents")
shares_everyone_ownsex_r_merged = add_shares_from_lm(everyone_res_r_within_merged, 
                                                       data=everyone_est_data_merged, 
                                                       dev_type = "own",
                                                     data_type = "nonparents")
shares_everyone_oppsex_merged = add_shares_from_lm(everyone_res_opposite_merged, 
                                                     data=everyone_est_data_merged, 
                                                     dev_type = "opp",
                                                   data_type = "nonparents")
shares_everyone_oppsex_r_merged = add_shares_from_lm(everyone_res_r_opposite_merged, 
                                                       data=everyone_est_data_merged, 
                                                       dev_type = "opp",
                                                     data_type = "nonparents")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
everyone_unres_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                         data = everyone_est_data_2015)
# excluding sleep and personal care
everyone_unres_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = everyone_est_data_2015)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
everyone_unres_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                          data = everyone_est_data_2015)
# excluding sleep and personal care
everyone_unres_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                            data = everyone_est_data_2015)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
everyone_unres_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                           data = everyone_est_data_2015)
# excluding sleep and personal care
everyone_unres_r_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                             data = everyone_est_data_2015)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
everyone_res_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                       data = everyone_est_data_2015,
                                       restrict.regMat = np_modReg_within)
# excluding sleep and personal care
everyone_res_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                         data = everyone_est_data_2015, 
                                         restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
everyone_res_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                        data = everyone_est_data_2015,
                                        restrict.regMat = np_modReg_between)
# excluding sleep and personal care
everyone_res_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                          data = everyone_est_data_2015,
                                          restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
everyone_res_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                         data = everyone_est_data_2015,
                                         restrict.regMat = np_modReg_opposite)
# excluding sleep and personal care
everyone_res_r_opposite_2015 = systemfit(np_eqns_r_opposite, method = "SUR", 
                                           data = everyone_est_data_2015,
                                           restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_everyone_bothsex_2015 = add_shares_from_lm(everyone_res_between_2015, 
                                                    data=everyone_est_data_2015, 
                                                    dev_type = "all",
                                                  data_type = "nonparents")
shares_everyone_bothsex_r_2015 = add_shares_from_lm(everyone_res_r_between_2015, 
                                                      data=everyone_est_data_2015, 
                                                      dev_type = "all",
                                                    data_type = "nonparents")
shares_everyone_ownsex_2015 = add_shares_from_lm(everyone_res_within_2015, 
                                                   data=everyone_est_data_2015, 
                                                   dev_type = "own",
                                                 data_type = "nonparents")
shares_everyone_ownsex_r_2015 = add_shares_from_lm(everyone_res_r_within_2015, 
                                                     data=everyone_est_data_2015, 
                                                     dev_type = "own",
                                                   data_type = "nonparents")
shares_everyone_oppsex_2015 = add_shares_from_lm(everyone_res_opposite_2015, 
                                                   data=everyone_est_data_2015, 
                                                   dev_type = "opp",
                                                 data_type = "nonparents")
shares_everyone_oppsex_r_2015 = add_shares_from_lm(everyone_res_r_opposite_2015, 
                                                     data=everyone_est_data_2015, 
                                                     dev_type = "opp",
                                                   data_type = "nonparents")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
everyone_unres_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                         data = everyone_est_data_2000)
# excluding sleep and personal care
everyone_unres_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = everyone_est_data_2000)

### No restrictions on coefficients: deviations from average of men AND women

# including sleep and personal care
everyone_unres_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                          data = everyone_est_data_2000)
# excluding sleep and personal care
everyone_unres_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                            data = everyone_est_data_2000)

### No restrictions on coefficients: deviations from average of opposite sex

# including sleep and personal care
everyone_unres_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                           data = everyone_est_data_2000)
# excluding sleep and personal care
everyone_unres_r_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                             data = everyone_est_data_2000)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
everyone_res_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                       data = everyone_est_data_2000,
                                       restrict.regMat = np_modReg_within)
# excluding sleep and personal care
everyone_res_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                         data = everyone_est_data_2000, 
                                         restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# including sleep and personal care
everyone_res_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                        data = everyone_est_data_2000,
                                        restrict.regMat = np_modReg_between)
# excluding sleep and personal care
everyone_res_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                          data = everyone_est_data_2000,
                                          restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# including sleep and personal care
everyone_res_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                         data = everyone_est_data_2000,
                                         restrict.regMat = np_modReg_opposite)
# excluding sleep and personal care
everyone_res_r_opposite_2000 = systemfit(np_eqns_r_opposite, method = "SUR", 
                                           data = everyone_est_data_2000,
                                           restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_everyone_bothsex_2000 = add_shares_from_lm(everyone_res_between_2000, 
                                                    data=everyone_est_data_2000, 
                                                    dev_type = "all",
                                                  data_type = "nonparents")
shares_everyone_bothsex_r_2000 = add_shares_from_lm(everyone_res_r_between_2000, 
                                                      data=everyone_est_data_2000, 
                                                      dev_type = "all",
                                                    data_type = "nonparents")
shares_everyone_ownsex_2000 = add_shares_from_lm(everyone_res_within_2000, 
                                                   data=everyone_est_data_2000, 
                                                   dev_type = "own",
                                                 data_type = "nonparents")
shares_everyone_ownsex_r_2000 = add_shares_from_lm(everyone_res_r_within_2000, 
                                                     data=everyone_est_data_2000, 
                                                     dev_type = "own",
                                                   data_type = "nonparents")
shares_everyone_oppsex_2000 = add_shares_from_lm(everyone_res_opposite_2000, 
                                                   data=everyone_est_data_2000, 
                                                   dev_type = "opp",
                                                 data_type = "nonparents")
shares_everyone_oppsex_r_2000 = add_shares_from_lm(everyone_res_r_opposite_2000, 
                                                     data=everyone_est_data_2000, 
                                                     dev_type = "opp",
                                                   data_type = "nonparents")

