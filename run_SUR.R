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

nonparents_est_data_merged = bind_rows(
  nonparents_est_data_2000 |> mutate(sample = "2000"), 
  nonparents_est_data_2015 |> mutate(sample = "2015"))

everyone_est_data_merged = bind_rows(
  nonparents_est_data_merged |> mutate(is_parent = 0),
  parents_est_data_merged |> mutate(is_parent = 1))

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
                                    data = parents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_within_merged = systemfit(p_eqns_r_within, method = "SUR", 
                                      data = parents_est_data_merged)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
parents_unres_between_merged = systemfit(p_eqns_between, method = "SUR", 
                                     data = parents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_between_merged = systemfit(p_eqns_r_between, method = "SUR", 
                                       data = parents_est_data_merged)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_unres_opposite_merged = systemfit(p_eqns_opposite, method = "SUR", 
                                      data = parents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_opposite_merged = systemfit(p_eqns_opposite, method = "SUR", 
                                        data = parents_est_data_merged)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
parents_res_within_merged = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_merged,
                                  restrict.regMat = p_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_within_merged = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_merged, 
                                    restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
parents_res_between_merged = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_merged,
                                   restrict.regMat = p_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_between_merged = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_merged,
                                     restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_res_opposite_merged = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_merged,
                                    restrict.regMat = p_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_opposite_merged = systemfit(p_eqns_r_opposite, method = "SUR", 
                                      data = parents_est_data_merged,
                                      restrict.regMat = p_modReg_opposite)

### calculate resource shares 

shares_parents_bothsex_merged = add_shares_from_lm(parents_res_between_merged, 
                                               data=parents_est_data_merged, 
                                               dev_type = "all",
                                               data_type = "parents")
shares_parents_bothsex_r_merged = add_shares_from_lm(parents_res_r_between_merged, 
                                                 data=parents_est_data_merged, 
                                                 dev_type = "all",
                                                 data_type = "parents")
shares_parents_ownsex_merged = add_shares_from_lm(parents_res_within_merged, 
                                              data=parents_est_data_merged, 
                                              dev_type = "own",
                                              data_type = "parents")
shares_parents_ownsex_r_merged = add_shares_from_lm(parents_res_r_within_merged, 
                                                data=parents_est_data_merged, 
                                                dev_type = "own",
                                                data_type = "parents")
shares_parents_oppsex_merged = add_shares_from_lm(parents_res_opposite_merged, 
                                              data=parents_est_data_merged, 
                                              dev_type = "opp",
                                              data_type = "parents")
shares_parents_oppsex_r_merged = add_shares_from_lm(parents_res_r_opposite_merged, 
                                                data=parents_est_data_merged, 
                                                dev_type = "opp",
                                                data_type = "parents")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
parents_unres_within_2015 = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_within_2015 = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_2015)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
parents_unres_between_2015 = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_between_2015 = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_2015)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_unres_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                      data = parents_est_data_2015)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
parents_res_within_2015 = systemfit(p_eqns_within, method = "SUR", 
                                data = parents_est_data_2015,
                                restrict.regMat = p_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_within_2015 = systemfit(p_eqns_r_within, method = "SUR", 
                                  data = parents_est_data_2015, 
                                  restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
parents_res_between_2015 = systemfit(p_eqns_between, method = "SUR", 
                                 data = parents_est_data_2015,
                                 restrict.regMat = p_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_between_2015 = systemfit(p_eqns_r_between, method = "SUR", 
                                   data = parents_est_data_2015,
                                   restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_res_opposite_2015 = systemfit(p_eqns_opposite, method = "SUR", 
                                  data = parents_est_data_2015,
                                  restrict.regMat = p_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_opposite_2015 = systemfit(p_eqns_r_opposite, method = "SUR", 
                                    data = parents_est_data_2015,
                                    restrict.regMat = p_modReg_opposite)

### calculate resource shares

shares_parents_bothsex_2015 = add_shares_from_lm(parents_res_between_2015, 
                                             data=parents_est_data_2015, 
                                             dev_type = "all",
                                             data_type = "parents")
shares_parents_bothsex_r_2015 = add_shares_from_lm(parents_res_r_between_2015, 
                                               data=parents_est_data_2015, 
                                               dev_type = "all",
                                               data_type = "parents")
shares_parents_ownsex_2015 = add_shares_from_lm(parents_res_within_2015, 
                                            data=parents_est_data_2015, 
                                            dev_type = "own",
                                            data_type = "parents")
shares_parents_ownsex_r_2015 = add_shares_from_lm(parents_res_r_within_2015, 
                                              data=parents_est_data_2015, 
                                              dev_type = "own",
                                              data_type = "parents")
shares_parents_oppsex_2015 = add_shares_from_lm(parents_res_opposite_2015, 
                                            data=parents_est_data_2015, 
                                            dev_type = "opp",
                                            data_type = "parents")
shares_parents_oppsex_r_2015 = add_shares_from_lm(parents_res_r_opposite_2015, 
                                              data=parents_est_data_2015, 
                                              dev_type = "opp",
                                              data_type = "parents")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
parents_unres_within_2000 = systemfit(p_eqns_within, method = "SUR", 
                                  data = parents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_within_2000 = systemfit(p_eqns_r_within, method = "SUR", 
                                    data = parents_est_data_2000)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
parents_unres_between_2000 = systemfit(p_eqns_between, method = "SUR", 
                                   data = parents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_between_2000 = systemfit(p_eqns_r_between, method = "SUR", 
                                     data = parents_est_data_2000)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_unres_opposite_2000 = systemfit(p_eqns_opposite, method = "SUR", 
                                    data = parents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_unres_r_opposite_2000 = systemfit(p_eqns_opposite, method = "SUR", 
                                      data = parents_est_data_2000)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
parents_res_within_2000 = systemfit(p_eqns_within, method = "SUR", 
                                data = parents_est_data_2000,
                                restrict.regMat = p_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_within_2000 = systemfit(p_eqns_r_within, method = "SUR", 
                                  data = parents_est_data_2000, 
                                  restrict.regMat = p_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
parents_res_between_2000 = systemfit(p_eqns_between, method = "SUR", 
                                 data = parents_est_data_2000,
                                 restrict.regMat = p_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_between_2000 = systemfit(p_eqns_r_between, method = "SUR", 
                                   data = parents_est_data_2000,
                                   restrict.regMat = p_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
parents_res_opposite_2000 = systemfit(p_eqns_opposite, method = "SUR", 
                                  data = parents_est_data_2000,
                                  restrict.regMat = p_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
parents_res_r_opposite_2000 = systemfit(p_eqns_r_opposite, method = "SUR", 
                                    data = parents_est_data_2000,
                                    restrict.regMat = p_modReg_opposite)

### calculate resource shares

shares_parents_bothsex_2000 = add_shares_from_lm(parents_res_between_2000, 
                                             data=parents_est_data_2000, 
                                             dev_type = "all",
                                             data_type = "parents")
shares_parents_bothsex_r_2000 = add_shares_from_lm(parents_res_r_between_2000, 
                                               data=parents_est_data_2000, 
                                               dev_type = "all",
                                               data_type = "parents")
shares_parents_ownsex_2000 = add_shares_from_lm(parents_res_within_2000, 
                                            data=parents_est_data_2000, 
                                            dev_type = "own",
                                            data_type = "parents")
shares_parents_ownsex_r_2000 = add_shares_from_lm(parents_res_r_within_2000, 
                                              data=parents_est_data_2000, 
                                              dev_type = "own",
                                              data_type = "parents")
shares_parents_oppsex_2000 = add_shares_from_lm(parents_res_opposite_2000, 
                                            data=parents_est_data_2000, 
                                            dev_type = "opp",
                                            data_type = "parents")
shares_parents_oppsex_r_2000 = add_shares_from_lm(parents_res_r_opposite_2000, 
                                              data=parents_est_data_2000, 
                                              dev_type = "opp",
                                              data_type = "parents")

############################ Counterfactual analysis ###########################

# the 2000 sharing rule using the 2015 data
counterfactual_2000sharing_2015data = add_shares_from_lm(
  parents_res_within_2015, data=parents_est_data_2000, dev_type = "own",
  data_type = "parents")

# the 2015 sharing rule using the 2000 data
counterfactual_2015sharing_2000data = add_shares_from_lm(
  parents_res_within_2000, data=parents_est_data_2015, dev_type = "own",
  data_type = "parents")

# the main sharing rule (estimated using both years) using the 2000 data
counterfactual_mainsharing_2000data = add_shares_from_lm(
  parents_res_within_merged, data=parents_est_data_2000, dev_type = "own",
  data_type = "parents")

# the main sharing rule (estimated using both years) using the 2015 data
counterfactual_mainsharing_2015data = add_shares_from_lm(
  parents_res_within_merged, data=parents_est_data_2015, dev_type = "own",
  data_type = "parents")

# the 2000 sharing rule using all data
counterfactual_2000sharing_alldata = add_shares_from_lm(
  parents_res_within_2000, data=parents_est_data_merged, dev_type = "own",
  data_type = "parents")

# the 2015 sharing rule using all data
counterfactual_2015sharing_alldata = add_shares_from_lm(
  parents_res_within_2015, data=parents_est_data_merged, dev_type = "own",
  data_type = "parents")

################################################################################
################################## NON-PARENTS #################################
################################################################################

############################### Using both years ###############################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_unres_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                    data = nonparents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                      data = nonparents_est_data_merged)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_unres_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                     data = nonparents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                       data = nonparents_est_data_merged)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_unres_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                        data = nonparents_est_data_merged)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_res_within_merged = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_merged,
                                  restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_within_merged = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_merged, 
                                    restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_res_between_merged = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_merged,
                                   restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_between_merged = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_merged,
                                     restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_res_opposite_merged = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_merged,
                                    restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_opposite_merged = systemfit(np_eqns_r_opposite, method = "SUR", 
                                      data = nonparents_est_data_merged,
                                      restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_nonparents_bothsex_merged = add_shares_from_lm(nonparents_res_between_merged, 
                                               data=nonparents_est_data_merged, 
                                               dev_type = "all",
                                               data_type = "nonparents")
shares_nonparents_bothsex_r_merged = add_shares_from_lm(nonparents_res_r_between_merged, 
                                                 data=nonparents_est_data_merged, 
                                                 dev_type = "all",
                                                 data_type = "nonparents")
shares_nonparents_ownsex_merged = add_shares_from_lm(nonparents_res_within_merged, 
                                              data=nonparents_est_data_merged, 
                                              dev_type = "own",
                                              data_type = "nonparents")
shares_nonparents_ownsex_r_merged = add_shares_from_lm(nonparents_res_r_within_merged, 
                                                data=nonparents_est_data_merged, 
                                                dev_type = "own",
                                                data_type = "nonparents")
shares_nonparents_oppsex_merged = add_shares_from_lm(nonparents_res_opposite_merged, 
                                              data=nonparents_est_data_merged, 
                                              dev_type = "opp",
                                              data_type = "nonparents")
shares_nonparents_oppsex_r_merged = add_shares_from_lm(nonparents_res_r_opposite_merged, 
                                                data=nonparents_est_data_merged, 
                                                dev_type = "opp",
                                                data_type = "nonparents")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_unres_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_2015)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_unres_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_2015)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_unres_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_2015)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_res_within_2015 = systemfit(np_eqns_within, method = "SUR", 
                                data = nonparents_est_data_2015,
                                restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_within_2015 = systemfit(np_eqns_r_within, method = "SUR", 
                                  data = nonparents_est_data_2015, 
                                  restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_res_between_2015 = systemfit(np_eqns_between, method = "SUR", 
                                 data = nonparents_est_data_2015,
                                 restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_between_2015 = systemfit(np_eqns_r_between, method = "SUR", 
                                   data = nonparents_est_data_2015,
                                   restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_res_opposite_2015 = systemfit(np_eqns_opposite, method = "SUR", 
                                  data = nonparents_est_data_2015,
                                  restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_opposite_2015 = systemfit(np_eqns_r_opposite, method = "SUR", 
                                    data = nonparents_est_data_2015,
                                    restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_nonparents_bothsex_2015 = add_shares_from_lm(nonparents_res_between_2015, 
                                             data=nonparents_est_data_2015, 
                                             dev_type = "all",
                                             data_type = "nonparents")
shares_nonparents_bothsex_r_2015 = add_shares_from_lm(nonparents_res_r_between_2015, 
                                               data=nonparents_est_data_2015, 
                                               dev_type = "all",
                                               data_type = "nonparents")
shares_nonparents_ownsex_2015 = add_shares_from_lm(nonparents_res_within_2015, 
                                            data=nonparents_est_data_2015, 
                                            dev_type = "own",
                                            data_type = "nonparents")
shares_nonparents_ownsex_r_2015 = add_shares_from_lm(nonparents_res_r_within_2015, 
                                              data=nonparents_est_data_2015, 
                                              dev_type = "own",
                                              data_type = "nonparents")
shares_nonparents_oppsex_2015 = add_shares_from_lm(nonparents_res_opposite_2015, 
                                            data=nonparents_est_data_2015, 
                                            dev_type = "opp",
                                            data_type = "nonparents")
shares_nonparents_oppsex_r_2015 = add_shares_from_lm(nonparents_res_r_opposite_2015, 
                                              data=nonparents_est_data_2015, 
                                              dev_type = "opp",
                                              data_type = "nonparents")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_unres_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                  data = nonparents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_2000)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_unres_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                   data = nonparents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_2000)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_unres_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_unres_r_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_2000)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
nonparents_res_within_2000 = systemfit(np_eqns_within, method = "SUR", 
                                data = nonparents_est_data_2000,
                                restrict.regMat = np_modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_within_2000 = systemfit(np_eqns_r_within, method = "SUR", 
                                  data = nonparents_est_data_2000, 
                                  restrict.regMat = np_modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
nonparents_res_between_2000 = systemfit(np_eqns_between, method = "SUR", 
                                 data = nonparents_est_data_2000,
                                 restrict.regMat = np_modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_between_2000 = systemfit(np_eqns_r_between, method = "SUR", 
                                   data = nonparents_est_data_2000,
                                   restrict.regMat = np_modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
nonparents_res_opposite_2000 = systemfit(np_eqns_opposite, method = "SUR", 
                                  data = nonparents_est_data_2000,
                                  restrict.regMat = np_modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
nonparents_res_r_opposite_2000 = systemfit(np_eqns_r_opposite, method = "SUR", 
                                    data = nonparents_est_data_2000,
                                    restrict.regMat = np_modReg_opposite)

### calculate resource shares

shares_nonparents_bothsex_2000 = add_shares_from_lm(nonparents_res_between_2000, 
                                             data=nonparents_est_data_2000, 
                                             dev_type = "all",
                                             data_type = "nonparents")
shares_nonparents_bothsex_r_2000 = add_shares_from_lm(nonparents_res_r_between_2000, 
                                               data=nonparents_est_data_2000, 
                                               dev_type = "all",
                                               data_type = "nonparents")
shares_nonparents_ownsex_2000 = add_shares_from_lm(nonparents_res_within_2000, 
                                            data=nonparents_est_data_2000, 
                                            dev_type = "own",
                                            data_type = "nonparents")
shares_nonparents_ownsex_r_2000 = add_shares_from_lm(nonparents_res_r_within_2000, 
                                              data=nonparents_est_data_2000, 
                                              dev_type = "own",
                                              data_type = "nonparents")
shares_nonparents_oppsex_2000 = add_shares_from_lm(nonparents_res_opposite_2000, 
                                            data=nonparents_est_data_2000, 
                                            dev_type = "opp",
                                            data_type = "nonparents")
shares_nonparents_oppsex_r_2000 = add_shares_from_lm(nonparents_res_r_opposite_2000, 
                                              data=nonparents_est_data_2000, 
                                              dev_type = "opp",
                                              data_type = "nonparents")

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

