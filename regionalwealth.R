library(tidyverse)
library(stringr)
library(fredr)
setwd(Sys.getenv("THESIS_WD"))

test = fredr(series_id = "ALPOP")
test2 = fredr(series_id = "ALNGSP")

state_codes = c(state.abb, "DC")



