library(tidyverse)
library(stringr)

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

################################# Working hours ################################

pay_to_weekly = function(pay, period) {
  case_when(
    period == 1 ~ pay,             # week
    period == 2 ~ pay / 2,          # two weeks
    period == 3 ~ pay / 4,          # four weeks
    period == 4 ~ pay / 4.333,      # calendar month
    period == 5 ~ pay / 52,         # year
    TRUE ~ NA_real_
  )
}

############################## Parents and couples #############################

# find spouse/partner pairs in data
find_spouse_pairs = function(relationships_data) {
  spouse_pairs = relationships_data |> 
    filter(pnum_is_spouse) |> 
    distinct(serial, pnum, relevant_person) |> 
    # keep only pairs of spouses/partners
    group_by(serial) |>
    filter(n() == 2) |>
    ungroup() |>
    # rename as spouse_pnum 
    rename(spouse_pnum = relevant_person) |> 
    arrange(serial, pnum)
  
  return(spouse_pairs)
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

# calculate resource shares with sharing rule
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
  addcols[[paste0(prefix_out, dev_type, "_etahat_m")]] = etahat_m_h
  addcols[[paste0(prefix_out, dev_type, "_etahat_f")]] = etahat_f_h
  
  # return both the augmented data AND the parameters used
  return(list(
    data = addcols,
    params = list(
      dev_type = dev_type,
      dev_map = dev_map,
      betahat0_m = betahat0_m,
      betahat0_f = betahat0_f,
      etahat0_m = etahat0_m,
      etahat0_f = etahat0_f,
      alphahat_l = alphahat_l,
      etahat_z_m = etahat_z_m,
      etahat_z_f = etahat_z_f)
  ))
}

################################### Plotting ###################################

# kernel density plots
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
    select(all_of(c(col_m, col_f))) |>
    pivot_longer(cols = everything(),
                        names_to = "sex",
                        values_to = "share") |>
    mutate(sex = case_when(sex == col_m ~ "Male",
                           sex == col_f ~ "Female"))
  
  # leisure excluding sleep and personal care
  if (restrict_leisure == FALSE) {
    label = "Including personal care and sleep"
  } else {
    label = "Excluding personal care and sleep"
  }
  
  # label 
  if (dev_type == "opp") {
    dev_label = "opposite"
  } else {
    dev_label = dev_type
  }
  
  ggplot(plot_data, aes(x = share, fill = sex, colour = sex)) +
    geom_density(alpha = alpha, bw = bw, linewidth = 1, na.rm = TRUE) +
    labs(
      x = "Estimated resource share",
      y = "Density",
      fill = NULL,
      colour = NULL,
      title = label) +
    xlim(0, 1) +
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

# get estimated coefficients
get_sysfit_stats = function(fit, coef_name) {
  b = fit$coefficients[coef_name]
  
  # If coefficient is missing (e.g., dropped), return NAs cleanly
  if (is.na(b) || length(b) == 0) {
    return(list(est = NA_real_, se = NA_real_, p = NA_real_))
  }
  
  V = fit$coefCov
  se = sqrt(V[coef_name, coef_name])
  
  # get p value
  z = b / se
  p = 2 * (1 - pnorm(abs(z)))
  
  list(est = unname(b), se = unname(se), p = unname(p))
}

# create dataframe with estimated coefficients
make_sysfit_kable_df = function(fit, row_map,
                                male_prefix = "male_",
                                female_prefix = "female_",
                                digits = 3) {
  
  fmt_num = function(x) {
    if (is.na(x)) return("")
    formatC(x, format = "g", digits = digits)
  }
  
  fmt_cell = function(est, se, p) {
    if (is.na(est) || is.na(se) || is.na(p)) return("")
    paste0(fmt_num(est), stars(p), " (", fmt_num(se), ")")
  }
  
  out = tibble(
    term = unname(row_map),
    male = NA_character_,
    female = NA_character_
  )
  
  base_terms = names(row_map)
  # print(base_terms)
  
  for (i in seq_along(base_terms)) {
    t = base_terms[i]
    
    m_name = paste0(male_prefix, t)
    f_name = paste0(female_prefix, t)
    
    m = get_sysfit_stats(fit, m_name)
    f = get_sysfit_stats(fit, f_name)
    
    out$male[i]   = fmt_cell(m$est, m$se, m$p)
    out$female[i] = fmt_cell(f$est, f$se, f$p)
  }
  
  out
}

# table of marginal effects
make_fem_share_impact_table = function(res,
                                        data,
                                        rows_map,
                                        mean_sd_vars = list(
                                          wage_m = "wage_m",
                                          wage_f = "wage_f",
                                          avgage = "avgage",
                                          agegap = "agegap",
                                          annual_income = "annual_income",  # your regional wealth proxy
                                          educ_m = "educ_m",
                                          educ_f = "educ_f"
                                        ),
                                        units = list(
                                          wage_gbp = 1,          # +£1 hourly wage
                                          age_years = 10,        # +10 years
                                          income_gbp = 5000,     # +£5000 annual income / regional wealth
                                          educ_levels = 1        # +1 education level
                                        ),
                                        pretty_names = c(
                                          wage_m = "Male wage (2024 GBP)",
                                          wage_f = "Female wage (2024 GBP)",
                                          avgage = "Average age of couple",
                                          agegap = "Age gap (male - female)",
                                          annual_income = "Regional wealth p.c. (2024 GBP)",
                                          educ_m = "Male qualifications (0/1/2)",
                                          educ_f = "Female qualifications (0/1/2)"
                                        ),
                                        dev_term_for = list(
                                          # map “table variable” -> which dev_* term drives the share rule
                                          wage_m = "dev_wage_m_only",
                                          wage_f = "dev_wage_f_only",
                                          avgage = "dev_avgage",
                                          agegap = "dev_agegap",
                                          annual_income = "dev_gdppc",
                                          educ_m = "dev_educ_m_only",
                                          educ_f = "dev_educ_f_only"
                                        ),
                                        clamp01 = FALSE) {
  stopifnot(is.list(res), !is.null(res$params), !is.null(res$params$dev_map))
  
  params = res$params
  
  # etahat_z_f is named by Bx_* terms; dev_map maps Bx_* -> dev_* column name
  theta_f_by_Bx = params$etahat_z_f
  dev_map = params$dev_map  # named vector: Bx_term -> dev_col
  
  # invert dev_map to go from dev_col -> Bx_term
  bx_for_dev = setNames(names(dev_map), unname(dev_map))
  
  # convenience: compute means/sds for “raw variables”
  msd = tibble(
    key = names(mean_sd_vars),
    var = unname(mean_sd_vars)
  ) |>
    mutate(
      mean = map_dbl(var, ~ mean(data[[.x]], na.rm = TRUE)),
      sd   = map_dbl(var, ~ stats::sd(data[[.x]], na.rm = TRUE))
    )
  
  # build impacts row-by-row
  out = msd |>
    mutate(
      variable = coalesce(pretty_names[key], key),
      
      # which deviation term corresponds to this row?
      dev_col = map_chr(key, ~ dev_term_for[[.x]] %||% NA_character_),
      bx_term = bx_for_dev[dev_col],
      theta_f = unname(theta_f_by_Bx[bx_term]),
      
      # impact of 1 SD increase in the deviation term on female share
      # (sd of dev_col, NOT sd of the raw variable)
      sd_dev = map_dbl(dev_col, ~ {
        if (is.na(.x) || !.x %in% names(data)) return(NA_real_)
        sd(data[[.x]], na.rm = TRUE)
      }),
      impact_1sd_on_fem_share = theta_f * sd_dev
    ) |>
    mutate(
      # “impact of …” custom units column
      impact_of = case_when(
        key %in% c("wage_m", "wage_f") ~ theta_f * units$wage_gbp,
        key %in% c("avgage", "agegap") ~ theta_f * units$age_years,
        key == "annual_income" ~ theta_f * units$income_gbp,
        key %in% c("educ_m", "educ_f") ~ theta_f * units$educ_levels,
        TRUE ~ NA_real_
      )
    ) |>
    select(variable, mean, sd, impact_1sd_on_fem_share, impact_of)
  
  return(out)
}

`%||%` = function(x, y) if (!is.null(x)) x else y


########################## Archive: import FRED data ###########################

# import data
fetch_fred_series = function(series_id) {
  fredr(series_id = series_id) |>
    transmute(date, value)}