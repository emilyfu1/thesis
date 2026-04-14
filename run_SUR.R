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

nonparents_est_data_merged = bind_rows(
  nonparents_est_data_2000 |> mutate(sample = "2000"), 
  nonparents_est_data_2015 |> mutate(sample = "2015"))

nonparents_est_data_merged_weekday = bind_rows(
  nonparents_est_data_2000_weekday |> mutate(sample = "2000"), 
  nonparents_est_data_2015_weekday |> mutate(sample = "2015"))

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

# leisure including eating, drinking, washing
parents_unres_within_merged = systemfit(p_eqns_within, method = "SUR", 
                                    data = parents_est_data_merged_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_within_merged = systemfit(p_eqns_r_within, method = "SUR", 
                                      data = parents_est_data_merged_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
parents_unres_between_merged = systemfit(p_eqns_between, method = "SUR", 
                                     data = parents_est_data_merged_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_between_merged = systemfit(p_eqns_r_between, method = "SUR", 
                                       data = parents_est_data_merged_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_unres_opposite_merged = systemfit(p_eqns_opposite, method = "SUR", 
                                      data = parents_est_data_merged_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_opposite_merged = systemfit(p_eqns_opposite, method = "SUR", 
                                        data = parents_est_data_merged_weekday)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
parents_res_within_merged = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_merged_weekday,
                                  restrict.regMat = p_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_within_merged = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_merged_weekday, 
                                    restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
parents_res_between_merged = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_merged_weekday,
                                   restrict.regMat = p_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_between_merged = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_merged_weekday,
                                     restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_res_opposite_merged = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_merged_weekday,
                                    restrict.regMat = p_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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

# leisure including eating, drinking, washing
parents_unres_within_2015 = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_2015_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_within_2015 = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_2015_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
parents_unres_between_2015 = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_2015_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_between_2015 = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_2015_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_unres_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_2015_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                      data = parents_est_data_2015_weekday)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
parents_res_within_2015 = systemfit(p_eqns_within, method = "SUR", 
                                data = parents_est_data_2015_weekday,
                                restrict.regMat = p_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_within_2015 = systemfit(p_eqns_r_within, method = "SUR", 
                                  data = parents_est_data_2015_weekday, 
                                  restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
parents_res_between_2015 = systemfit(p_eqns_between, method = "SUR", 
                                 data = parents_est_data_2015_weekday,
                                 restrict.regMat = p_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_between_2015 = systemfit(p_eqns_r_between, method = "SUR", 
                                   data = parents_est_data_2015_weekday,
                                   restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_res_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                  data = parents_est_data_2015_weekday,
                                  restrict.regMat = p_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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

# leisure including eating, drinking, washing
parents_unres_within_2000 = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_2000_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_within_2000 = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_2000_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
parents_unres_between_2000 = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_2000_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_between_2000 = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_2000_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_unres_opposite_2000 = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_2000_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_opposite_2000 = systemfit(p_eqns_opposite, method = "SUR", 
                                      data = parents_est_data_2000_weekday)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
parents_res_within_2000 = systemfit(p_eqns_within, method = "SUR", 
                                data = parents_est_data_2000_weekday,
                                restrict.regMat = p_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_within_2000 = systemfit(p_eqns_r_within, method = "SUR", 
                                  data = parents_est_data_2000_weekday, 
                                  restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
parents_res_between_2000 = systemfit(p_eqns_between, method = "SUR", 
                                 data = parents_est_data_2000_weekday,
                                 restrict.regMat = p_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_between_2000 = systemfit(p_eqns_r_between, method = "SUR", 
                                   data = parents_est_data_2000_weekday,
                                   restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_res_opposite_2000 = systemfit(p_eqns_opposite, method = "SUR", 
                                  data = parents_est_data_2000_weekday,
                                  restrict.regMat = p_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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

# the 2000 sharing rule using the 2015 data
counterfactual_2000sharing_2015data = add_shares_from_lm(
  parents_res_within_2015, data=parents_est_data_2000_weekday, dev_type = "own",
  data_type = "parents")

# the 2015 sharing rule using the 2000 data
counterfactual_2015sharing_2000data = add_shares_from_lm(
  parents_res_within_2000, data=parents_est_data_2015_weekday, dev_type = "own",
  data_type = "parents")

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

########################## merge with sharing_est_data #########################

parents_est_data_merged_shares = inner_join(shares_parents_ownsex_r_merged$data,
                                            parents_est_data_merged,
                                            by=c('serial')) |>
  rename(shareown_etahat_f_r = shareown_etahat_f,
         shareown_etahat_m_r = shareown_etahat_m) |>
  inner_join(shares_parents_ownsex_merged$data, by=c('serial')) |>
  mutate(share_budget_leisure_r = (private_leisure_exp_r_f + 
                                     private_leisure_exp_r_m) / y,
         share_budget_leisure = (private_leisure_exp_f + 
                                   private_leisure_exp_m) / y)

###################### merge with individual/day level data ####################

# individual-day level data
reg_stacked = bind_rows(
  data_working_parents_2000 |>
    transmute(
      serial,
      sample = "2000",
      dgorpaf,
      pnum,
      is_weekend = as.integer(is_weekend), # dummy
      male = as.integer(male), # dummy
      has_childcare_help = as.integer(has_childcare_help),
      has_otherdomestic_help = as.integer(has_otherdomestic_help),
      wage, 
      total_work,
      total_otherdomestic,
      total_private_leisure,
      total_private_leisure_r,
      total_childcare_nospouse,
      total_childcare,
      private_leisure_exp,
      private_leisure_exp_r),
  data_working_parents_2015 |>
    transmute(
      serial,
      dgorpaf,
      pnum,
      sample = "2015",
      is_weekend = as.integer(is_weekend),
      male = as.integer(male),
      has_childcare_help = as.integer(has_childcare_help),
      has_otherdomestic_help = as.integer(has_otherdomestic_help),
      wage,
      total_work,
      total_otherdomestic,
      total_private_leisure,
      total_private_leisure_r,
      total_childcare_nospouse,
      total_childcare,
      private_leisure_exp,
      private_leisure_exp_r)) |>
  # time dummies
  mutate(dummy_2000 = ifelse(sample == "2000", 1, 0),
         dummy_2015 = ifelse(sample == "2015", 1, 0),
         day_type = if_else(is_weekend == 1, "weekend", "weekday")) |>
  
  # merge with resource share estimates
  inner_join(parents_est_data_merged_shares, 
             by=c("serial", "sample", "dgorpaf", "is_weekend")) |>
  
  # wage ratio
  mutate(wage_ratio_f = wage_f / (wage_m + wage_f)) |>
  
  # only personal care
  mutate(total_private_personalcare = total_private_leisure - total_private_leisure_r) |>
  
  # individual identifiers
  group_by(sample, serial, pnum) |>
  mutate(individual_id = row_number()) |>
  ungroup() |>
  
  # child under five in household
  mutate(child_under_five = ifelse(kid_age_min <= 5, 1, 0),
         proportion_male_children = num_kids_male / num_kids_total,
         has_young_child = n_kid_aged_0_2 + n_kid_aged_3_5 + n_kid_aged_6_10 > 0)

# individual level data
reg_individual = reg_stacked |>
  group_by(sample, serial, pnum) |>
  mutate(
    
    # weekday and weekend levels
    work_weekday = total_work[is_weekend == 0][1],
    work_weekend = total_work[is_weekend == 1][1],
    
    otherdom_weekday = total_otherdomestic[is_weekend == 0][1],
    otherdom_weekend = total_otherdomestic[is_weekend == 1][1],
    
    pleisure_weekday = total_private_leisure[is_weekend == 0][1],
    pleisure_weekend = total_private_leisure[is_weekend == 1][1],
    
    pleisure_r_weekday = total_private_leisure_r[is_weekend == 0][1],
    pleisure_r_weekend = total_private_leisure_r[is_weekend == 1][1],
    
    childcare_ns_weekday = total_childcare_nospouse[is_weekend == 0][1],
    childcare_ns_weekend = total_childcare_nospouse[is_weekend == 1][1],
    
    childcare_weekday = total_childcare[is_weekend == 0][1],
    childcare_weekend = total_childcare[is_weekend == 1][1],
    
    pleisure_exp_weekday = private_leisure_exp[is_weekend == 0][1],
    pleisure_exp_weekend = private_leisure_exp[is_weekend == 1][1],
    
    pleisure_exp_r_weekday = private_leisure_exp_r[is_weekend == 0][1],
    pleisure_exp_r_weekend = private_leisure_exp_r[is_weekend == 1][1],
    
    # weekend - weekday differences
    d_work = work_weekend - work_weekday,
    d_otherdomestic = otherdom_weekend - otherdom_weekday,
    d_private_leisure = pleisure_weekend - pleisure_weekday,
    d_private_leisure_r = pleisure_r_weekend - pleisure_r_weekday,
    d_childcare_nospouse = childcare_ns_weekend - childcare_ns_weekday,
    d_childcare = childcare_weekend - childcare_weekday,
    d_private_leisure_exp = pleisure_exp_weekend - pleisure_exp_weekday,
    d_private_leisure_exp_r = pleisure_exp_r_weekend - pleisure_exp_r_weekday
  ) |>
  ungroup() |>
  filter(is_weekend == 1) |>
  ungroup()

################################################################################
################################## NON-PARENTS #################################
################################################################################

############################### Using both years ###############################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_unres_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                    data = nonparents_est_data_merged_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                      data = nonparents_est_data_merged_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_unres_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                     data = nonparents_est_data_merged_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                       data = nonparents_est_data_merged_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_unres_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_merged_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                        data = nonparents_est_data_merged_weekday)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_res_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_merged_weekday,
                                  restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_merged_weekday, 
                                    restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_res_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_merged_weekday,
                                   restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_merged_weekday,
                                     restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_res_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_merged_weekday,
                                    restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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

# leisure including eating, drinking, washing
nonparents_unres_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_2015_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_2015_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_unres_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_2015_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_2015_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_unres_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_2015_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_2015_weekday)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_res_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                data = nonparents_est_data_2015_weekday,
                                restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                  data = nonparents_est_data_2015_weekday, 
                                  restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_res_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                 data = nonparents_est_data_2015_weekday,
                                 restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                   data = nonparents_est_data_2015_weekday,
                                   restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_res_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                  data = nonparents_est_data_2015_weekday,
                                  restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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

# leisure including eating, drinking, washing
nonparents_unres_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_2000_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_2000_weekday)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_unres_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_2000_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_2000_weekday)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_unres_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_2000_weekday)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_2000_weekday)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_res_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                data = nonparents_est_data_2000_weekday,
                                restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                  data = nonparents_est_data_2000_weekday, 
                                  restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_res_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                 data = nonparents_est_data_2000_weekday,
                                 restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                   data = nonparents_est_data_2000_weekday,
                                   restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_res_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                  data = nonparents_est_data_2000_weekday,
                                  restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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
  rename(shareown_etahat_f_r = shareown_etahat_f,
         shareown_etahat_m_r = shareown_etahat_m) |>
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

# leisure including eating, drinking, washing
everyone_unres_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                           data = everyone_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                             data = everyone_est_data_merged)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
everyone_unres_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                            data = everyone_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                              data = everyone_est_data_merged)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
everyone_unres_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                             data = everyone_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                               data = everyone_est_data_merged)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
everyone_res_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                         data = everyone_est_data_merged,
                                         restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_res_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = everyone_est_data_merged, 
                                           restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
everyone_res_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                          data = everyone_est_data_merged,
                                          restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_res_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                            data = everyone_est_data_merged,
                                            restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
everyone_res_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                           data = everyone_est_data_merged,
                                           restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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

# leisure including eating, drinking, washing
everyone_unres_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                         data = everyone_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = everyone_est_data_2015)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
everyone_unres_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                          data = everyone_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                            data = everyone_est_data_2015)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
everyone_unres_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                           data = everyone_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                             data = everyone_est_data_2015)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
everyone_res_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                       data = everyone_est_data_2015,
                                       restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_res_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                         data = everyone_est_data_2015, 
                                         restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
everyone_res_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                        data = everyone_est_data_2015,
                                        restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_res_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                          data = everyone_est_data_2015,
                                          restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
everyone_res_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                         data = everyone_est_data_2015,
                                         restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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

# leisure including eating, drinking, washing
everyone_unres_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                         data = everyone_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = everyone_est_data_2000)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
everyone_unres_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                          data = everyone_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                            data = everyone_est_data_2000)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
everyone_unres_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                           data = everyone_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_unres_r_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                             data = everyone_est_data_2000)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
everyone_res_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                       data = everyone_est_data_2000,
                                       restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_res_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                         data = everyone_est_data_2000, 
                                         restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
everyone_res_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                        data = everyone_est_data_2000,
                                        restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
everyone_res_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                          data = everyone_est_data_2000,
                                          restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
everyone_res_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                         data = everyone_est_data_2000,
                                         restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
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

