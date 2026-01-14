library(tidyverse)
library(stringr)
setwd(Sys.getenv("THESIS_WD"))

# significance stars
stars = function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  if (p < 0.1) return(".")
  return("")}

# import data
fetch_fred_series = function(series_id) {
  fredr(series_id = series_id) |>
    transmute(date, value)}

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
                              male_prefix = "male:",
                              female_prefix = "female:",
                              y_term_m = "male:y",
                              y_term_f = "y:female",
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
  
  if (dev_type == "own") {
    dev_map = dev_map_ownsex
  } else {
    dev_map = dev_map_bothsex
  }
  
  coefficients = fit$coefficients
  nm = names(coefficients)
  
  betahat0_m = unname(coefficients[y_term_m])
  betahat0_f = unname(coefficients[y_term_f])
  sum_betahat0_mf = betahat0_m + betahat0_f
  # find average resource share for men and women
  etahat0_m = betahat0_m / sum_betahat0_mf
  etahat0_f = betahat0_f / sum_betahat0_mf
  
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
  addcols = data
  addcols[[paste0(prefix_out, dev_type, "_etahat0_m")]] = etahat0_m
  addcols[[paste0(prefix_out, dev_type, "_etahat0_f")]] = etahat0_f
  addcols[[paste0(prefix_out, dev_type, "_alphahat_l")]] = alphahat_l
  addcols[[paste0(prefix_out, dev_type, "_etahat_m")]] = etahat_m_h
  addcols[[paste0(prefix_out, dev_type, "_etahat_f")]] = etahat_f_h
  
  return (addcols)
}

plot_share_densities = function(data,
                                dev_type,
                                prefix = "share",
                                bw = "nrd0",
                                alpha = 0.5) {
  
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
  
  ggplot(plot_data, aes(x = share, fill = sex, colour = sex)) +
    geom_density(alpha = alpha, bw = bw, linewidth = 1) +
    labs(
      x = "Estimated resource share",
      y = "Density",
      fill = NULL,
      colour = NULL,
      title = paste("Estimated resource shares (", dev_type, "-sex deviations)", sep = "")
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
}