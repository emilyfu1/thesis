library(tidyverse)
library(systemfit)

# actlines and directories
source("UKTUS_params.R")

# functions
source("functions.R")

# setwd
setwd(wd)

# import data (saves time running code)
sharing_est_data_2015 = read_csv(paste0(data_direct, 
                                        "sharing_est_data_2015.csv"),
                                 show_col_types = FALSE)
sharing_est_data_2000 = read_csv(paste0(data_direct, 
                                        "sharing_est_data_2000.csv"),
                                 show_col_types = FALSE)


# run a single regression
get_SUR_output = function(eqns, data, R = NULL) {
  if (is.null(R)) {
    systemfit(eqns, method = "SUR", data = data)
  } else {
    systemfit(eqns, method = "SUR", data = data, restrict.regMat = R)
  }
}

# all my data
mydata = list(
  `2000` = sharing_est_data_2000,
  `2015` = sharing_est_data_2015,
  pooled = bind_rows(
    sharing_est_data_2000 |> mutate(sample = "2000"),
    sharing_est_data_2015 |> mutate(sample = "2015")))

# nested list: results[[dataset]][[spec]][[version]]
results = imap(mydata, \(dat, dname) {
  imap(specs, \(sp, sname) {
    list(
      unres = list(
        broad = get_SUR_output(sp$eq, dat, R = NULL),
        restr = get_SUR_output(sp$eq_r, dat, R = NULL)
      ),
      res = list(
        broad = get_SUR_output(sp$eq, dat, R = sp$R),
        restr = get_SUR_output(sp$eq_r, dat, R = sp$R)))
  })
})

# results[["2015"]][["within"]][["unres"]][["broad"]]