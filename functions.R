library(tidyverse)
library(stringr)
setwd(Sys.getenv("THESIS_WD"))

################################## Activities ##################################

# find which month to keep in the individual and household data
unique_interview_months = function(data) {
  diarymonth_households = data |>
    distinct(serial, pnum, IMonth) |>
    arrange(serial, pnum)
  return(diarymonth_households)
}

# find time diaries and number of diaries everyone completes
unique_interview_diaries = function(data) {
  individual_diaries = data |>
    distinct(serial, pnum, DiaryDay_Act) |>
    group_by(serial, pnum) |>
    # household budget: i need to indicate how many days everyone has completed
    # so that i calculate expenditure and budget based on number of days
    mutate(num_diaries_filled = n()) |>
    distinct(serial, pnum, num_diaries_filled)
  return(individual_diaries)
}

################################### Children ###################################

# find kids in data
find_kids = function(relationships_data, individual_data) {
  # find all children in data with sex and age (includes adult kids)
  data_kids = relationships_data |>
    filter(pnum_is_child) |>
    # get identifiers
    distinct(serial, pnum) |>
    # get individual characteristics
    inner_join(individual_data, by = c("serial", "pnum")) |>
    select(serial, pnum, DVAge, DMSex)
  
  return(data_kids)
}

# cound number of children by sex
count_kids = function(data) {
  num_kids = data |>
    group_by(serial) |>
    summarise(num_kids_total = n(),
              num_kids_male = sum(DMSex == 1, na.rm = TRUE),
              num_kids_female = sum(DMSex == 2, na.rm = TRUE),
              .groups = "drop")
  return(num_kids)
}

# get age distribution of children
find_kid_ages = function(data) {
  kids_age_dist = data |>
    group_by(serial) |>
    summarise(kid_age_min  = min(DVAge, na.rm = TRUE),
              kid_age_max  = max(DVAge, na.rm = TRUE),
              kid_age_mean = mean(DVAge, na.rm = TRUE),
              
              n_kid_aged_0_2 = sum(DVAge <= 2, na.rm = TRUE),
              n_kid_aged_3_5 = sum(DVAge >= 3  & DVAge <= 5, na.rm = TRUE),
              n_kid_aged_6_10 = sum(DVAge >= 6  & DVAge <= 10, na.rm = TRUE),
              n_kid_aged_11_13  = sum(DVAge >= 11 & DVAge <= 13, na.rm = TRUE),
              n_kid_aged_14_17 = sum(DVAge >= 14 & DVAge <= 17, na.rm = TRUE),
              .groups = "drop") |>
    select(serial, kid_age_min, kid_age_max, kid_age_mean, n_kid_aged_0_2,
           n_kid_aged_3_5, n_kid_aged_6_10, n_kid_aged_11_13, n_kid_aged_14_17)
  
  return(kids_age_dist)
}

# all ages of children
find_kid_ages_wide = function(data) {
  kids_age_wide = data |>
    arrange(serial, desc(DVAge)) |>
    group_by(serial) |>
    mutate(kid_index = row_number()) |>
    ungroup() |>
    select(serial, kid_index, DVAge) |>
    pivot_wider(names_from = kid_index,
                values_from = DVAge,
                names_prefix = "age_of_kid_")
  
  return(kids_age_wide)
}


################################## Regressions #################################
# matrix of restrictions for SUREs
make_regMat = function(regressors, theta_names,
                       y_names = c("male_y", "female_y"),
                       eq_names = c("male", "female")) {
  
  stopifnot(length(eq_names) == 2, length(y_names) == 2)
  stopifnot(length(theta_names) == length(regressors))
  
  # this is what the regression coefficients will turn out to be
  rownames_mat = c(
    y_names[1],
    paste0(eq_names[1], "_", regressors),
    y_names[2],
    paste0(eq_names[2], "_", regressors))
  
  # 14 rows (coefficients) x 8 columns (what the restriction has to be)
  
  colnames_mat = c(y_names, theta_names)
  
  regMat = matrix(0, nrow = length(rownames_mat), ncol = length(colnames_mat),
                   dimnames = list(rownames_mat, colnames_mat))
  
  # household budget coefficients map to their own parameters (unrestricted)
  regMat[y_names[1], y_names[1]] = 1
  regMat[y_names[2], y_names[2]] = 1
  
  # impose symmetry constraints: male = +theta, female = -theta
  for (i in seq_along(regressors)) {
    r = regressors[i]
    th = theta_names[i]
    regMat[paste0(eq_names[1], "_", r), th] =  1
    regMat[paste0(eq_names[2], "_", r), th] = -1
    }
  
  regMat
}

add_shares_from_lm = function(fit, data,
                              dev_type,
                              male_prefix = "male_",
                              female_prefix = "female_",
                              y_term_m = "male_y",
                              y_term_f = "female_y",
                              clamp01 = FALSE,
                              prefix_out = "share") {
  dev_map_ownsex = c("Bx_dev_wage_f_only" = "dev_wage_f_only",
                     "Bx_dev_wage_m_only" = "dev_wage_m_only",
                     "Bx_dev_educ_f_only" = "dev_educ_f_only",
                     "Bx_dev_educ_m_only" = "dev_educ_m_only",
                     "Bx_dev_avgage" = "dev_avgage",
                     "Bx_dev_agegap" = "dev_agegap",
                     "Bx_dev_gdppc" = "dev_gdppc")
  dev_map_bothsex = c("Bx_dev_wage_f_all" = "dev_wage_f_all",
                      "Bx_dev_wage_m_all" = "dev_wage_m_all",
                      "Bx_dev_educ_f_all" = "dev_educ_f_all",
                      "Bx_dev_educ_m_all" = "dev_educ_m_all",
                      "Bx_dev_avgage" = "dev_avgage",
                      "Bx_dev_agegap" = "dev_agegap",
                      "Bx_dev_gdppc" = "dev_gdppc")
  dev_map_oppositesex = c("Bx_dev_wage_f_opp" = "dev_wage_f_opp",
                          "Bx_dev_wage_m_opp" = "dev_wage_m_opp",
                           "Bx_dev_educ_f_opp" = "dev_educ_f_opp",
                           "Bx_dev_educ_m_opp" = "dev_educ_m_opp",
                           "Bx_dev_avgage" = "dev_avgage",
                           "Bx_dev_agegap" = "dev_agegap",
                           "Bx_dev_gdppc" = "dev_gdppc")
  
  if (dev_type == "own") {
    dev_map = dev_map_ownsex
  } else if (dev_type == "all") {
    dev_map = dev_map_bothsex
  } else if (dev_type == "opp") {
    dev_map = dev_map_oppositesex
  }
  
  coefficients = fit$coefficients
  nm = names(coefficients)
  
  # print(coefficients)
  # print(nm)
  
  betahat0_m = unname(coefficients[y_term_m])
  betahat0_f = unname(coefficients[y_term_f])
  sum_betahat0_mf = betahat0_m + betahat0_f
  # find average resource share for men and women
  etahat0_m = betahat0_m / sum_betahat0_mf
  etahat0_f = betahat0_f / sum_betahat0_mf
  
  # print(etahat0_m)
  # print(etahat0_f)
  
  # the cobb doublas preference parameter is given by beta0_t / eta_t
  # but this is just sum_beta0_mf
  alphahat_l = sum_betahat0_mf
  
  # print(nm)
  
  # helper function for string names
  get_beta_z = function(sex_prefix, suffix) {
    key = paste0(sex_prefix, suffix)
    if (!key %in% nm) return(NA_real_)
    unname(coefficients[key])
  }
  
  # now, find influence of each deviation thing for each gender
  etahat_z_m = vapply(names(dev_map),
                      function(suf) get_beta_z(male_prefix, suf) / alphahat_l, 
                      numeric(1))
  etahat_z_f = vapply(names(dev_map), 
                      function(suf) get_beta_z(female_prefix, suf) / alphahat_l, 
                      numeric(1))
  
  # matrix of deviation variables in data (columns align with dev_map order)
  dev_mat = as.matrix(data[, unname(dev_map), drop = FALSE])
  etahat_m_h = as.numeric(etahat0_m + dev_mat %*% etahat_z_m)
  etahat_f_h = as.numeric(etahat0_f + dev_mat %*% etahat_z_f)
  
  # restricting proportion sizes essentially
  if (clamp01) {
    etahat_m_h = pmin(pmax(etahat_m_h, 0), 1)
    etahat_f_h = pmin(pmax(etahat_f_h, 0), 1)
  }
  
  # add outputs
  addcols = data[c("serial")]
  addcols[[paste0(prefix_out, dev_type, "_etahat0_m")]] = etahat0_m
  addcols[[paste0(prefix_out, dev_type, "_etahat0_f")]] = etahat0_f
  addcols[[paste0(prefix_out, dev_type, "_alphahat_l")]] = alphahat_l
  addcols[[paste0(prefix_out, dev_type, "_etahat_m")]] = etahat_m_h
  addcols[[paste0(prefix_out, dev_type, "_etahat_f")]] = etahat_f_h
  
  return (addcols)
}

################################### Plotting ###################################

plot_share_densities = function(data,
                                dev_type,
                                prefix = "share",
                                bw = "nrd0",
                                alpha = 0.5,
                                restrict_leisure = TRUE) {
  
  # construct column names
  col_m = paste0(prefix, dev_type, "_etahat_m")
  col_f = paste0(prefix, dev_type, "_etahat_f")
  
  # reshape to long for ggplot
  plot_data = data |>
    dplyr::select(all_of(c(col_m, col_f))) |>
    tidyr::pivot_longer(cols = everything(),
                        names_to = "sex",
                        values_to = "share") |>
    dplyr::mutate(sex = dplyr::case_when(sex == col_m ~ "Male",
                                         sex == col_f ~ "Female"))
  
  # leisure excluding sleep and personal care
  if (restrict_leisure == FALSE) {
    label = "Estimated resource shares"
  } else {
    label = "Estimated resource shares (excluding personal care and sleep)"
  }
  
  # label 
  if (dev_type == "opp") {
    dev_label = "opposite"
  } else {
    dev_label = dev_type
  }
  
  ggplot(plot_data, aes(x = share, fill = sex, colour = sex)) +
    geom_density(alpha = alpha, bw = bw, linewidth = 1) +
    labs(
      x = paste0(label, " (using )", dev_label, "-sex deviations)", sep = ""),
      y = "Density",
      fill = NULL,
      colour = NULL,
      title = label
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
}

# significance stars
stars = function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  if (p < 0.1) return(".")
  return("")}

########################## Archive: import FRED data ###########################

# import data
fetch_fred_series = function(series_id) {
  fredr(series_id = series_id) |>
    transmute(date, value)}