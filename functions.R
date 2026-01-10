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