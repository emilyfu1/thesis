library(tidyverse)
library(systemfit)

# actlines and directories
source("UKTUS_params.R")

# functions
source("functions.R")

# setwd
setwd(wd)

# data
source("datacleaning_UKTUS_2015.R")
source("datacleaning_UKTUS_2000.R")

parents_est_data_merged = bind_rows(
  parents_est_data_2000 |> mutate(sample = "2000"), 
  parents_est_data_2015 |> mutate(sample = "2015"))

nonparents_est_data_merged = bind_rows(
  nonparents_est_data_2000 |> mutate(sample = "2000"), 
  nonparents_est_data_2015 |> mutate(sample = "2015"))

# this is a super long block of code! work in progress to make it more efficient

################################################################################
#################################### PARENTS ###################################
################################################################################

############################### Using both years ###############################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
fit_unres_within_merged = systemfit(eqns_within, method = "SUR", 
                                    data = parents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_within_merged = systemfit(eqns_r_within, method = "SUR", 
                                      data = parents_est_data_merged)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
fit_unres_between_merged = systemfit(eqns_between, method = "SUR", 
                                     data = parents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_between_merged = systemfit(eqns_r_between, method = "SUR", 
                                       data = parents_est_data_merged)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_unres_opposite_merged = systemfit(eqns_opposite, method = "SUR", 
                                      data = parents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_opposite_merged = systemfit(eqns_opposite, method = "SUR", 
                                        data = parents_est_data_merged)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
fit_res_within_merged = systemfit(eqns_within, method = "SUR", 
                                  data = parents_est_data_merged,
                                  restrict.regMat = modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_within_merged = systemfit(eqns_r_within, method = "SUR", 
                                    data = parents_est_data_merged, 
                                    restrict.regMat = modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
fit_res_between_merged = systemfit(eqns_between, method = "SUR", 
                                   data = parents_est_data_merged,
                                   restrict.regMat = modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_between_merged = systemfit(eqns_r_between, method = "SUR", 
                                     data = parents_est_data_merged,
                                     restrict.regMat = modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_res_opposite_merged = systemfit(eqns_opposite, method = "SUR", 
                                    data = parents_est_data_merged,
                                    restrict.regMat = modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_opposite_merged = systemfit(eqns_r_opposite, method = "SUR", 
                                      data = parents_est_data_merged,
                                      restrict.regMat = modReg_opposite)

shares_fit_bothsex_merged = add_shares_from_lm(fit_res_between_merged, 
                                               data=parents_est_data_merged, 
                                               dev_type = "all")
shares_fit_bothsex_r_merged = add_shares_from_lm(fit_res_r_between_merged, 
                                                 data=parents_est_data_merged, 
                                                 dev_type = "all")
shares_fit_ownsex_merged = add_shares_from_lm(fit_res_within_merged, 
                                              data=parents_est_data_merged, 
                                              dev_type = "own")
shares_fit_ownsex_r_merged = add_shares_from_lm(fit_res_r_within_merged, 
                                                data=parents_est_data_merged, 
                                                dev_type = "own")
shares_fit_oppsex_merged = add_shares_from_lm(fit_res_opposite_merged, 
                                              data=parents_est_data_merged, 
                                              dev_type = "opp")
shares_fit_oppsex_r_merged = add_shares_from_lm(fit_res_r_opposite_merged, 
                                                data=parents_est_data_merged, 
                                                dev_type = "opp")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
fit_unres_within_2015 = systemfit(eqns_within, method = "SUR", 
                                  data = parents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_within_2015 = systemfit(eqns_r_within, method = "SUR", 
                                    data = parents_est_data_2015)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
fit_unres_between_2015 = systemfit(eqns_between, method = "SUR", 
                                   data = parents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_between_2015 = systemfit(eqns_r_between, method = "SUR", 
                                     data = parents_est_data_2015)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_unres_opposite_2015 = systemfit(eqns_opposite, method = "SUR", 
                                    data = parents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_opposite_2015 = systemfit(eqns_opposite, method = "SUR", 
                                      data = parents_est_data_2015)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
fit_res_within_2015 = systemfit(eqns_within, method = "SUR", 
                                data = parents_est_data_2015,
                                restrict.regMat = modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_within_2015 = systemfit(eqns_r_within, method = "SUR", 
                                  data = parents_est_data_2015, 
                                  restrict.regMat = modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
fit_res_between_2015 = systemfit(eqns_between, method = "SUR", 
                                 data = parents_est_data_2015,
                                 restrict.regMat = modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_between_2015 = systemfit(eqns_r_between, method = "SUR", 
                                   data = parents_est_data_2015,
                                   restrict.regMat = modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_res_opposite_2015 = systemfit(eqns_opposite, method = "SUR", 
                                  data = parents_est_data_2015,
                                  restrict.regMat = modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_opposite_2015 = systemfit(eqns_r_opposite, method = "SUR", 
                                    data = parents_est_data_2015,
                                    restrict.regMat = modReg_opposite)

shares_fit_bothsex_2015 = add_shares_from_lm(fit_res_between_2015, 
                                             data=parents_est_data_2015, 
                                             dev_type = "all")
shares_fit_bothsex_r_2015 = add_shares_from_lm(fit_res_r_between_2015, 
                                               data=parents_est_data_2015, 
                                               dev_type = "all")
shares_fit_ownsex_2015 = add_shares_from_lm(fit_res_within_2015, 
                                            data=parents_est_data_2015, 
                                            dev_type = "own")
shares_fit_ownsex_r_2015 = add_shares_from_lm(fit_res_r_within_2015, 
                                              data=parents_est_data_2015, 
                                              dev_type = "own")
shares_fit_oppsex_2015 = add_shares_from_lm(fit_res_opposite_2015, 
                                            data=parents_est_data_2015, 
                                            dev_type = "opp")
shares_fit_oppsex_r_2015 = add_shares_from_lm(fit_res_r_opposite_2015, 
                                              data=parents_est_data_2015, 
                                              dev_type = "opp")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
fit_unres_within_2000 = systemfit(eqns_within, method = "SUR", 
                                  data = parents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_within_2000 = systemfit(eqns_r_within, method = "SUR", 
                                    data = parents_est_data_2000)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
fit_unres_between_2000 = systemfit(eqns_between, method = "SUR", 
                                   data = parents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_between_2000 = systemfit(eqns_r_between, method = "SUR", 
                                     data = parents_est_data_2000)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_unres_opposite_2000 = systemfit(eqns_opposite, method = "SUR", 
                                    data = parents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_opposite_2000 = systemfit(eqns_opposite, method = "SUR", 
                                      data = parents_est_data_2000)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
fit_res_within_2000 = systemfit(eqns_within, method = "SUR", 
                                data = parents_est_data_2000,
                                restrict.regMat = modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_within_2000 = systemfit(eqns_r_within, method = "SUR", 
                                  data = parents_est_data_2000, 
                                  restrict.regMat = modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
fit_res_between_2000 = systemfit(eqns_between, method = "SUR", 
                                 data = parents_est_data_2000,
                                 restrict.regMat = modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_between_2000 = systemfit(eqns_r_between, method = "SUR", 
                                   data = parents_est_data_2000,
                                   restrict.regMat = modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_res_opposite_2000 = systemfit(eqns_opposite, method = "SUR", 
                                  data = parents_est_data_2000,
                                  restrict.regMat = modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_opposite_2000 = systemfit(eqns_r_opposite, method = "SUR", 
                                    data = parents_est_data_2000,
                                    restrict.regMat = modReg_opposite)

shares_fit_bothsex_2000 = add_shares_from_lm(fit_res_between_2000, 
                                             data=parents_est_data_2000, 
                                             dev_type = "all")
shares_fit_bothsex_r_2000 = add_shares_from_lm(fit_res_r_between_2000, 
                                               data=parents_est_data_2000, 
                                               dev_type = "all")
shares_fit_ownsex_2000 = add_shares_from_lm(fit_res_within_2000, 
                                            data=parents_est_data_2000, 
                                            dev_type = "own")
shares_fit_ownsex_r_2000 = add_shares_from_lm(fit_res_r_within_2000, 
                                              data=parents_est_data_2000, 
                                              dev_type = "own")
shares_fit_oppsex_2000 = add_shares_from_lm(fit_res_opposite_2000, 
                                            data=parents_est_data_2000, 
                                            dev_type = "opp")
shares_fit_oppsex_r_2000 = add_shares_from_lm(fit_res_r_opposite_2000, 
                                              data=parents_est_data_2000, 
                                              dev_type = "opp")

############################ Counterfactual analysis ###########################

counterfactual_within = add_shares_from_lm(fit_res_within_2015, 
                                           data=parents_est_data_2000, 
                                           dev_type = "own")

################################################################################
################################## NON-PARENTS #################################
################################################################################

############################### Using both years ###############################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
fit_unres_within_merged = systemfit(eqns_within, method = "SUR", 
                                    data = nonparents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_within_merged = systemfit(eqns_r_within, method = "SUR", 
                                      data = nonparents_est_data_merged)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
fit_unres_between_merged = systemfit(eqns_between, method = "SUR", 
                                     data = nonparents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_between_merged = systemfit(eqns_r_between, method = "SUR", 
                                       data = nonparents_est_data_merged)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_unres_opposite_merged = systemfit(eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_merged)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_opposite_merged = systemfit(eqns_opposite, method = "SUR", 
                                        data = nonparents_est_data_merged)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
fit_res_within_merged = systemfit(eqns_within, method = "SUR", 
                                  data = nonparents_est_data_merged,
                                  restrict.regMat = modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_within_merged = systemfit(eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_merged, 
                                    restrict.regMat = modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
fit_res_between_merged = systemfit(eqns_between, method = "SUR", 
                                   data = nonparents_est_data_merged,
                                   restrict.regMat = modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_between_merged = systemfit(eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_merged,
                                     restrict.regMat = modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_res_opposite_merged = systemfit(eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_merged,
                                    restrict.regMat = modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_opposite_merged = systemfit(eqns_r_opposite, method = "SUR", 
                                      data = nonparents_est_data_merged,
                                      restrict.regMat = modReg_opposite)

shares_fit_bothsex_merged = add_shares_from_lm(fit_res_between_merged, 
                                               data=nonparents_est_data_merged, 
                                               dev_type = "all")
shares_fit_bothsex_r_merged = add_shares_from_lm(fit_res_r_between_merged, 
                                                 data=nonparents_est_data_merged, 
                                                 dev_type = "all")
shares_fit_ownsex_merged = add_shares_from_lm(fit_res_within_merged, 
                                              data=nonparents_est_data_merged, 
                                              dev_type = "own")
shares_fit_ownsex_r_merged = add_shares_from_lm(fit_res_r_within_merged, 
                                                data=nonparents_est_data_merged, 
                                                dev_type = "own")
shares_fit_oppsex_merged = add_shares_from_lm(fit_res_opposite_merged, 
                                              data=nonparents_est_data_merged, 
                                              dev_type = "opp")
shares_fit_oppsex_r_merged = add_shares_from_lm(fit_res_r_opposite_merged, 
                                                data=nonparents_est_data_merged, 
                                                dev_type = "opp")

################################## Using 2015 ##################################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
fit_unres_within_2015 = systemfit(eqns_within, method = "SUR", 
                                  data = nonparents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_within_2015 = systemfit(eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_2015)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
fit_unres_between_2015 = systemfit(eqns_between, method = "SUR", 
                                   data = nonparents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_between_2015 = systemfit(eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_2015)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_unres_opposite_2015 = systemfit(eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_2015)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_opposite_2015 = systemfit(eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_2015)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
fit_res_within_2015 = systemfit(eqns_within, method = "SUR", 
                                data = nonparents_est_data_2015,
                                restrict.regMat = modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_within_2015 = systemfit(eqns_r_within, method = "SUR", 
                                  data = nonparents_est_data_2015, 
                                  restrict.regMat = modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
fit_res_between_2015 = systemfit(eqns_between, method = "SUR", 
                                 data = nonparents_est_data_2015,
                                 restrict.regMat = modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_between_2015 = systemfit(eqns_r_between, method = "SUR", 
                                   data = nonparents_est_data_2015,
                                   restrict.regMat = modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_res_opposite_2015 = systemfit(eqns_opposite, method = "SUR", 
                                  data = nonparents_est_data_2015,
                                  restrict.regMat = modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_opposite_2015 = systemfit(eqns_r_opposite, method = "SUR", 
                                    data = nonparents_est_data_2015,
                                    restrict.regMat = modReg_opposite)

shares_fit_bothsex_2015 = add_shares_from_lm(fit_res_between_2015, 
                                             data=nonparents_est_data_2015, 
                                             dev_type = "all")
shares_fit_bothsex_r_2015 = add_shares_from_lm(fit_res_r_between_2015, 
                                               data=nonparents_est_data_2015, 
                                               dev_type = "all")
shares_fit_ownsex_2015 = add_shares_from_lm(fit_res_within_2015, 
                                            data=nonparents_est_data_2015, 
                                            dev_type = "own")
shares_fit_ownsex_r_2015 = add_shares_from_lm(fit_res_r_within_2015, 
                                              data=nonparents_est_data_2015, 
                                              dev_type = "own")
shares_fit_oppsex_2015 = add_shares_from_lm(fit_res_opposite_2015, 
                                            data=nonparents_est_data_2015, 
                                            dev_type = "opp")
shares_fit_oppsex_r_2015 = add_shares_from_lm(fit_res_r_opposite_2015, 
                                              data=nonparents_est_data_2015, 
                                              dev_type = "opp")

################################## Using 2000 ##################################

### No restrictions on coefficients: deviations from average with own sex

# leisure including eating, drinking, washing
fit_unres_within_2000 = systemfit(eqns_within, method = "SUR", 
                                  data = nonparents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_within_2000 = systemfit(eqns_r_within, method = "SUR", 
                                    data = nonparents_est_data_2000)

### No restrictions on coefficients: deviations from average of men AND women

# leisure including eating, drinking, washing
fit_unres_between_2000 = systemfit(eqns_between, method = "SUR", 
                                   data = nonparents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_between_2000 = systemfit(eqns_r_between, method = "SUR", 
                                     data = nonparents_est_data_2000)

### No restrictions on coefficients: deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_unres_opposite_2000 = systemfit(eqns_opposite, method = "SUR", 
                                    data = nonparents_est_data_2000)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_unres_r_opposite_2000 = systemfit(eqns_opposite, method = "SUR", 
                                      data = nonparents_est_data_2000)

### Restricted coefficients, deviations from average with own sex

# leisure including eating, drinking, washing
fit_res_within_2000 = systemfit(eqns_within, method = "SUR", 
                                data = nonparents_est_data_2000,
                                restrict.regMat = modReg_within)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_within_2000 = systemfit(eqns_r_within, method = "SUR", 
                                  data = nonparents_est_data_2000, 
                                  restrict.regMat = modReg_within)

### Restricted coefficients, deviations from average of men AND women

# leisure including eating, drinking, washing
fit_res_between_2000 = systemfit(eqns_between, method = "SUR", 
                                 data = nonparents_est_data_2000,
                                 restrict.regMat = modReg_between)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_between_2000 = systemfit(eqns_r_between, method = "SUR", 
                                   data = nonparents_est_data_2000,
                                   restrict.regMat = modReg_between)

### Restricted coefficients, deviations from average of opposite sex

# leisure including eating, drinking, washing
fit_res_opposite_2000 = systemfit(eqns_opposite, method = "SUR", 
                                  data = nonparents_est_data_2000,
                                  restrict.regMat = modReg_opposite)
# relaxing, socialising, sport, entertainment, and hobbies only
fit_res_r_opposite_2000 = systemfit(eqns_r_opposite, method = "SUR", 
                                    data = nonparents_est_data_2000,
                                    restrict.regMat = modReg_opposite)

shares_fit_bothsex_2000 = add_shares_from_lm(fit_res_between_2000, 
                                             data=nonparents_est_data_2000, 
                                             dev_type = "all")
shares_fit_bothsex_r_2000 = add_shares_from_lm(fit_res_r_between_2000, 
                                               data=nonparents_est_data_2000, 
                                               dev_type = "all")
shares_fit_ownsex_2000 = add_shares_from_lm(fit_res_within_2000, 
                                            data=nonparents_est_data_2000, 
                                            dev_type = "own")
shares_fit_ownsex_r_2000 = add_shares_from_lm(fit_res_r_within_2000, 
                                              data=nonparents_est_data_2000, 
                                              dev_type = "own")
shares_fit_oppsex_2000 = add_shares_from_lm(fit_res_opposite_2000, 
                                            data=nonparents_est_data_2000, 
                                            dev_type = "opp")
shares_fit_oppsex_r_2000 = add_shares_from_lm(fit_res_r_opposite_2000, 
                                              data=nonparents_est_data_2000, 
                                              dev_type = "opp")
