library(systemfit)

# actlines and directories
source("UKTUS_params.R")

# import data (saves time running code)
sharing_est_data_2015 = read_csv(paste0(data_direct, 
                                        "sharing_est_data_2015.csv"))
sharing_est_data_2000 = read_csv(paste0(data_direct, 
                                        "sharing_est_data_2000.csv"))

# setwd
setwd(wd)

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
        broad = run_SUR(sp$eq, dat, R = NULL),
        restr = run_SUR(sp$eq_r, dat, R = NULL)
      ),
      res = list(
        broad = run_SUR(sp$eq, dat, R = sp$R),
        restr = run_SUR(sp$eq_r, dat, R = sp$R)))
  })
})

# results[["2015"]][["within"]][["unres"]][["broad"]]