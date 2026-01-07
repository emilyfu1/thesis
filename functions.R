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
    transmute(date, value)
}