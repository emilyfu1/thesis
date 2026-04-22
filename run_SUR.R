################################# Merging data #################################

parents_est_data_merged = bind_rows(
  parents_est_data_2000 |> mutate(sample = "2000"), 
  parents_est_data_2015 |> mutate(sample = "2015"))

parents_est_data_merged_weekday = parents_est_data_merged |>
  filter(is_weekend == 0)

parents_est_data_merged_weekend = parents_est_data_merged |>
  filter(is_weekend == 1)

nonparents_est_data_merged = bind_rows(
  nonparents_est_data_2000 |> mutate(sample = "2000"), 
  nonparents_est_data_2015 |> mutate(sample = "2015"))

nonparents_est_data_merged_weekday = nonparents_est_data_merged |>
  filter(is_weekend == 0)

nonparents_est_data_merged_weekend = nonparents_est_data_merged |>
  filter(is_weekend == 1)

everyone_est_data_merged = bind_rows(
  nonparents_est_data_merged |> mutate(is_parent = 0),
  parents_est_data_merged |> mutate(is_parent = 1))

everyone_est_data_merged_weekday = everyone_est_data_merged |>
  filter(is_weekend == 0)

everyone_est_data_merged_weekend = everyone_est_data_merged |>
  filter(is_weekend == 1)

everyone_est_data_2000 = everyone_est_data_merged_weekday |> 
  filter(sample == "2000")
everyone_est_data_2015 = everyone_est_data_merged_weekday |> 
  filter(sample == "2015")


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

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
parents_res_within_merged = systemfit(p_eqns_within, method = "SUR", 
                                      data = parents_est_data_merged_weekday,
                                      restrict.regMat = p_modReg_within)
# excluding sleep and personal care
parents_res_r_within_merged = systemfit(p_eqns_r_within, method = "SUR", 
                                        data = parents_est_data_merged_weekday, 
                                        restrict.regMat = p_modReg_within)

### calculate resource shares 

shares_parents_ownsex_merged = add_shares_from_lm(parents_res_within_merged, 
                                                  data = parents_est_data_merged_weekday, 
                                                  data_type = "parents")
shares_parents_ownsex_r_merged = add_shares_from_lm(parents_res_r_within_merged, 
                                                    data = parents_est_data_merged_weekday, 
                                                    data_type = "parents")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
parents_unres_within_2015 = systemfit(p_eqns_within, method = "SUR", 
                                      data = parents_est_data_2015)
# excluding sleep and personal care
parents_unres_r_within_2015 = systemfit(p_eqns_r_within, method = "SUR", 
                                        data = parents_est_data_2015)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
parents_res_within_2015 = systemfit(p_eqns_within, method = "SUR", 
                                    data = parents_est_data_2015,
                                    restrict.regMat = p_modReg_within)
# excluding sleep and personal care
parents_res_r_within_2015 = systemfit(p_eqns_r_within, method = "SUR", 
                                      data = parents_est_data_2015, 
                                      restrict.regMat = p_modReg_within)

### calculate resource shares

shares_parents_ownsex_2015 = add_shares_from_lm(parents_res_within_2015, 
                                                data=parents_est_data_2015, 
                                                data_type = "parents")
shares_parents_ownsex_r_2015 = add_shares_from_lm(parents_res_r_within_2015, 
                                                  data=parents_est_data_2015, 
                                                  data_type = "parents")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
parents_unres_within_2000 = systemfit(p_eqns_within, method = "SUR", 
                                      data = parents_est_data_2000)
# excluding sleep and personal care
parents_unres_r_within_2000 = systemfit(p_eqns_r_within, method = "SUR", 
                                        data = parents_est_data_2000)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
parents_res_within_2000 = systemfit(p_eqns_within, method = "SUR", 
                                    data = parents_est_data_2000,
                                    restrict.regMat = p_modReg_within)
# excluding sleep and personal care
parents_res_r_within_2000 = systemfit(p_eqns_r_within, method = "SUR", 
                                      data = parents_est_data_2000, 
                                      restrict.regMat = p_modReg_within)

### calculate resource shares

shares_parents_ownsex_2000 = add_shares_from_lm(parents_res_within_2000, 
                                            data = parents_est_data_2000, 
                                            data_type = "parents")
shares_parents_ownsex_r_2000 = add_shares_from_lm(parents_res_r_within_2000, 
                                                  data = parents_est_data_2000, 
                                                  data_type = "parents")

####################### Special weekend-only sharing rule ######################

parents_res_weekend = systemfit(p_eqns_within, method = "SUR",
                                data = parents_est_data_merged_weekend,
                                restrict.regMat = p_modReg_within)

nonparents_res_weekend = systemfit(np_eqns_within, method = "SUR",
                                   data = nonparents_est_data_merged_weekend,
                                   restrict.regMat = np_modReg_within)

parents_res_r_weekend = systemfit(p_eqns_r_within, method = "SUR",
                                  data = parents_est_data_merged_weekend,
                                  restrict.regMat = p_modReg_within)

nonparents_res_r_weekend = systemfit(np_eqns_r_within, method = "SUR",
                                     data = nonparents_est_data_merged_weekend,
                                     restrict.regMat = np_modReg_within)

shares_parents_weekend = add_shares_from_lm(parents_res_weekend, 
                                            data = parents_est_data_merged_weekend, 
                                            data_type = "parents")

shares_nonparents_weekend = add_shares_from_lm(nonparents_res_weekend, 
                                               data = nonparents_est_data_merged_weekend, 
                                               data_type = "nonparents")

shares_parents_r_weekend = add_shares_from_lm(parents_res_r_weekend, 
                                            data = parents_est_data_merged_weekend, 
                                            data_type = "parents")

shares_nonparents_r_weekend = add_shares_from_lm(nonparents_res_r_weekend, 
                                               data = nonparents_est_data_merged_weekend, 
                                               data_type = "nonparents")

############################ Counterfactual analysis ###########################

# including sleep and personal care

# the 2000 sharing rule using the 2015 data
counterfactual_2000sharing_2015data = add_shares_from_lm(
  parents_res_within_2000, data = parents_est_data_2015,
   data_type = "parents")

# the 2015 sharing rule using the 2000 data
counterfactual_2015sharing_2000data = add_shares_from_lm(
  parents_res_within_2015, data = parents_est_data_2000,
   data_type = "parents")

# the main sharing rule (estimated using both years) using the 2000 data
counterfactual_mainsharing_2000data = add_shares_from_lm(
  parents_res_within_merged, data=parents_est_data_2000, 
  data_type = "parents")

# the main sharing rule (estimated using both years) using the 2015 data
counterfactual_mainsharing_2015data = add_shares_from_lm(
  parents_res_within_merged, data=parents_est_data_2015, 
  data_type = "parents")

# the 2000 sharing rule using all data
counterfactual_2000sharing_alldata = add_shares_from_lm(
  parents_res_within_2000, data=parents_est_data_merged_weekday, 
  data_type = "parents")

# the 2015 sharing rule using all data
counterfactual_2015sharing_alldata = add_shares_from_lm(
  parents_res_within_2015, data=parents_est_data_merged_weekday, 
  data_type = "parents")

# excluding sleep and personal care

# the 2000 sharing rule using the 2015 data
counterfactual_2000sharing_2015data_r = add_shares_from_lm(
  parents_res_r_within_2000, data = parents_est_data_2015,
   data_type = "parents")

# the 2015 sharing rule using the 2000 data
counterfactual_2015sharing_2000data_r = add_shares_from_lm(
  parents_res_r_within_2015, data = parents_est_data_2000,
   data_type = "parents")

# the main sharing rule (estimated using both years) using the 2000 data
counterfactual_mainsharing_2000data_r = add_shares_from_lm(
  parents_res_r_within_merged, data = parents_est_data_2000, 
  data_type = "parents")

# the main sharing rule (estimated using both years) using the 2015 data
counterfactual_mainsharing_2015data_r = add_shares_from_lm(
  parents_res_r_within_merged, data = parents_est_data_2015, 
  data_type = "parents")

# the 2000 sharing rule using all data
counterfactual_2000sharing_alldata_r = add_shares_from_lm(
  parents_res_r_within_2000, data = parents_est_data_merged_weekday, 
  data_type = "parents")

# the 2015 sharing rule using all data
counterfactual_2015sharing_alldata_r = add_shares_from_lm(
  parents_res_r_within_2015, data = parents_est_data_merged_weekday, 
  data_type = "parents")

########################## merge with sharing_est_data #########################

shares_tmp = systemfit(np_eqns_within, method = "SUR", 
                       data = parents_est_data_merged_weekday,
                       restrict.regMat = np_modReg_within)

shares_tmp_merged = add_shares_from_lm(shares_tmp, 
                                       data = parents_est_data_merged_weekday, 
                                       data_type = "nonparents")

shares_tmp_r = systemfit(np_eqns_r_within, method = "SUR", 
                       data = parents_est_data_merged_weekday,
                       restrict.regMat = np_modReg_within)

shares_tmp_merged_r = add_shares_from_lm(shares_tmp_r, 
                                       data = parents_est_data_merged_weekday, 
                                       data_type = "nonparents")

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
         num_kids_total, num0_2, num3_4, num5_9, num10_15, num16_17, 
         avgage, agegap_m,
         child_under_5, num_under_5, num_under_10, 
         child_under_10, has_childcare_help, has_otherdomestic_help) |>
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
      has_childcare_help, has_otherdomestic_help, wage, educ,
      total_work, total_otherdomestic, total_private_leisure, total_sleep,
      total_private_leisure_r, total_childcare, private_leisure_exp, 
      total_childcare_exp, total_otherdomestic_exp, private_leisure_exp_r,
      num_kids_total, num0_2, num3_4, num5_9, num10_15, num16_17,
      child_under_5, num_under_5, num_under_10, child_under_10),
  data_working_parents_2015 |>
    transmute(
      serial, sample = "2000", dgorpaf, pnum, is_weekend, male,
      has_childcare_help, has_otherdomestic_help, wage, educ,
      total_work, total_otherdomestic, total_private_leisure, total_sleep,
      total_private_leisure_r, total_childcare, private_leisure_exp, 
      total_childcare_exp, total_otherdomestic_exp, private_leisure_exp_r,
      num_kids_total, num0_2, num3_4, num5_9, num10_15, num16_17,
      child_under_5, num_under_5, num_under_10, child_under_10)) |>
  # time dummies
  mutate(dummy_2000 = ifelse(sample == "2000", 1, 0),
         dummy_2015 = ifelse(sample == "2015", 1, 0)) |>
  
  # merge with resource share estimates
  inner_join(parents_est_data_merged_shares |> select(serial, dgorpaf, is_weekend, wage_f, 
                                         wage_m, avgage, agegap_m, educ_f, educ_m,
                                         shareown_etahat_f, rgdppc,
                                         shareown_etahat_m,
                                         shareown_etahat_r_f, 
                                         shareown_etahat_r_m), 
             by=c("serial", "dgorpaf", "is_weekend")) |>
  
  mutate(over_75p_fem_share = if_else(shareown_etahat_f > quantile(shareown_etahat_f, 0.75), 1, 0),
         over_75p_fem_share_r = if_else(shareown_etahat_r_f > quantile(shareown_etahat_f, 0.75), 1, 0),
         wage_ratio_f = wage_f / (wage_f + wage_m),
         educ_gap_f = educ_f - educ_m)

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

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
nonparents_res_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                         data = nonparents_est_data_merged_weekday,
                                         restrict.regMat = np_modReg_within)
# excluding sleep and personal care
nonparents_res_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = nonparents_est_data_merged_weekday, 
                                           restrict.regMat = np_modReg_within)

### calculate resource shares

shares_nonparents_ownsex_merged = add_shares_from_lm(nonparents_res_within_merged, 
                                                     data=nonparents_est_data_merged_weekday, 
                                                     data_type = "nonparents")
shares_nonparents_ownsex_r_merged = add_shares_from_lm(nonparents_res_r_within_merged, 
                                                       data=nonparents_est_data_merged_weekday, 
                                                       data_type = "nonparents")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
nonparents_unres_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                         data = nonparents_est_data_2015)
# excluding sleep and personal care
nonparents_unres_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = nonparents_est_data_2015)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
nonparents_res_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                       data = nonparents_est_data_2015,
                                       restrict.regMat = np_modReg_within)
# excluding sleep and personal care
nonparents_res_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                         data = nonparents_est_data_2015, 
                                         restrict.regMat = np_modReg_within)

### calculate resource shares

shares_nonparents_ownsex_2015 = add_shares_from_lm(nonparents_res_within_2015, 
                                                   data=nonparents_est_data_2015, 
                                            
                                            data_type = "nonparents")
shares_nonparents_ownsex_r_2015 = add_shares_from_lm(nonparents_res_r_within_2015, 
                                                     data=nonparents_est_data_2015, 
                                                     data_type = "nonparents")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
nonparents_unres_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                         data = nonparents_est_data_2000)
# excluding sleep and personal care
nonparents_unres_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = nonparents_est_data_2000)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
nonparents_res_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                       data = nonparents_est_data_2000,
                                       restrict.regMat = np_modReg_within)
# excluding sleep and personal care
nonparents_res_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                         data = nonparents_est_data_2000, 
                                         restrict.regMat = np_modReg_within)

### calculate resource shares
shares_nonparents_ownsex_2000 = add_shares_from_lm(nonparents_res_within_2000, 
                                                   data=nonparents_est_data_2000, 
                                                   data_type = "nonparents")
shares_nonparents_ownsex_r_2000 = add_shares_from_lm(nonparents_res_r_within_2000, 
                                                     data=nonparents_est_data_2000, 
                                                     data_type = "nonparents")

########################## merge with sharing_est_data #########################

nonparents_est_data_merged_shares = inner_join(shares_nonparents_ownsex_r_merged$data,
                                               nonparents_est_data_merged,
                                               by=c('serial')) |>
  rename(shareown_etahat_r_f = shareown_etahat_f,
         shareown_etahat_r_m = shareown_etahat_m) |>
  inner_join(shares_nonparents_ownsex_merged$data, by=c('serial')) |>
  mutate(share_budget_leisure_r = (private_leisure_exp_r_f + 
                                     private_leisure_exp_r_m) / y,
         share_budget_leisure = (private_leisure_exp_f + 
                                   private_leisure_exp_m) / y)

# individual day level data
reg_stacked_nonparents = bind_rows(
  data_working_nonparents_2000 |>
    transmute(
      serial, sample = "2000", dgorpaf, pnum, is_weekend, male,
      has_otherdomestic_help, wage, educ,
      total_work, total_otherdomestic, total_private_leisure, total_sleep,
      total_private_leisure_r, total_childcare, private_leisure_exp, 
      total_childcare_exp, total_otherdomestic_exp, private_leisure_exp_r),
    data_working_nonparents_2015 |>
        transmute(
          serial, sample = "2000", dgorpaf, pnum, is_weekend, male,
          has_otherdomestic_help, wage, educ,
          total_work, total_otherdomestic, total_private_leisure, total_sleep,
          total_private_leisure_r, total_childcare, private_leisure_exp, 
          total_childcare_exp, total_otherdomestic_exp, private_leisure_exp_r)) |>
    # time dummies
    mutate(dummy_2000 = ifelse(sample == "2000", 1, 0),
           dummy_2015 = ifelse(sample == "2015", 1, 0)) |>
    
    # merge with resource share estimates
    inner_join(nonparents_est_data_merged_shares |> select(serial, dgorpaf, is_weekend, wage_f, 
                                                           wage_m, avgage, agegap_m, educ_f, educ_m,
                                                           shareown_etahat_f, rgdppc,
                                                           shareown_etahat_m,
                                                           shareown_etahat_r_f, 
                                                           shareown_etahat_r_m), 
               by=c("serial", "dgorpaf", "is_weekend")) |>
    
    mutate(over_75p_fem_share = if_else(shareown_etahat_f > quantile(shareown_etahat_f, 0.75), 1, 0),
           over_75p_fem_share_r = if_else(shareown_etahat_r_f > quantile(shareown_etahat_f, 0.75), 1, 0),
           wage_ratio_f = wage_f / (wage_f + wage_m),
           educ_gap_f = educ_f - educ_m)

################################################################################
###################################### BOTH ####################################
################################################################################

############################### Using both years ###############################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
everyone_unres_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                           data = everyone_est_data_merged_weekday)
# excluding sleep and personal care
everyone_unres_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                             data = everyone_est_data_merged_weekday)

### Restricted coefficients, deviations from average with own sex

# including sleep and personal care
everyone_res_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                         data = everyone_est_data_merged_weekday,
                                         restrict.regMat = np_modReg_within)
# excluding sleep and personal care
everyone_res_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                           data = everyone_est_data_merged_weekday, 
                                           restrict.regMat = np_modReg_within)

### calculate resource shares

shares_everyone_ownsex_merged = add_shares_from_lm(everyone_res_within_merged, 
                                                   data=everyone_est_data_merged_weekday, 
                                                   data_type = "nonparents")
shares_everyone_ownsex_r_merged = add_shares_from_lm(everyone_res_r_within_merged, 
                                                     data=everyone_est_data_merged_weekday, 
                                                     data_type = "nonparents")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
everyone_unres_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                       data = everyone_est_data_2015)
# excluding sleep and personal care
everyone_unres_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
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

### calculate resource shares

shares_everyone_ownsex_2015 = add_shares_from_lm(everyone_res_within_2015, 
                                                 data=everyone_est_data_2015, 
                                                 data_type = "nonparents")
shares_everyone_ownsex_r_2015 = add_shares_from_lm(everyone_res_r_within_2015, 
                                                   data=everyone_est_data_2015, 
                                                   data_type = "nonparents")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# including sleep and personal care
everyone_unres_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                       data = everyone_est_data_2000)
# excluding sleep and personal care
everyone_unres_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
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

### calculate resource shares

shares_everyone_ownsex_2000 = add_shares_from_lm(everyone_res_within_2000, 
                                                 data=everyone_est_data_2000, 
                                                 data_type = "nonparents")
shares_everyone_ownsex_r_2000 = add_shares_from_lm(everyone_res_r_within_2000, 
                                                   data=everyone_est_data_2000, 
                                                   data_type = "nonparents")

########################## merge with sharing_est_data #########################

everyone_est_data_merged_shares = inner_join(shares_everyone_ownsex_r_merged$data,
                                            everyone_est_data_merged,
                                            by=c('serial')) |>
  rename(shareown_etahat_r_f = shareown_etahat_f,
         shareown_etahat_r_m = shareown_etahat_m) |>
  inner_join(shares_everyone_ownsex_merged$data, by=c('serial')) |>
  mutate(share_budget_leisure_r = (private_leisure_exp_r_f + 
                                     private_leisure_exp_r_m) / y,
         share_budget_leisure = (private_leisure_exp_f + 
                                   private_leisure_exp_m) / y)

