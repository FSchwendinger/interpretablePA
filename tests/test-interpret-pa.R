# Checks for interpret.pa(): runs the real Shiny server via shiny::testServer() and compares
# its results with values computed directly from the COmPLETE reference models.
# Run with the package installed: Rscript tests/test-interpret-pa.R

suppressPackageStartupMessages(library(interpretablePA))

ml   <- interpretablePA:::model_list
mdl  <- function(metric, sex) ml[[paste0("centile_", metric, "_", sex)]]
med  <- function(metric, sex, age) centiles.pred(mdl(metric, sex), type = "centiles", xname = "Age", xvalues = age, cent = 50, calibration = FALSE)$`50`
z    <- function(metric, sex, age, y) z.scores(mdl(metric, sex), x = age, y = y)
near <- function(a, b, tol = 0.011) isTRUE(all(abs(a - b) <= tol))

# Run expr inside the app's server and return its value
run_server <- function(expr) {
  out <- new.env()
  shiny::testServer(interpret.pa(), { out$val <- eval(expr) })
  out$val
}
tmp_csv <- function(df) {
  path <- tempfile(fileext = ".csv")
  write.csv(df, path, row.names = FALSE)
  path
}

#------------------------------------------------------------------------------------------
# 1. Upload: every row is compared with its own sex-specific model and median
#------------------------------------------------------------------------------------------

up <- data.frame(ID = 1:6, avacc = c(30, 25, 40, 35, 28, 30), ig = c(-2.5, -2.6, -2.3, -2.4, -2.55, -2.5),
                 age = c(25, 40, 55, 70, 85, 50), sex = c("f", "female", "m", "f", "F", "m")) # last row male
sx <- c("f", "f", "m", "f", "f", "m")

calc_upload <- function(df) {
  path <- tmp_csv(df)
  run_server(bquote({
    session$setInputs(upload = data.frame(name = "u.csv", size = 1, type = "text/csv", datapath = .(path)))
    calc_50_perc_uploaded()
  }))
}

res <- calc_upload(up)
c50_avacc <- round(mapply(med, "avacc", sx, up$age), 2)
c50_ig    <- round(mapply(med, "ig", sx, up$age), 3)

stopifnot(
  near(res$cent50_avacc, c50_avacc),
  near(res$avacc_perc_pred, round(up$avacc / c50_avacc * 100, 2)),
  near(res$ig_perc_pred, 100 + round((c50_ig - up$ig) / c50_ig * 100, 2)),
  near(res$avacc_z, round(mapply(z, "avacc", sx, up$age, up$avacc), 2)),
  near(res$ig_z, round(mapply(z, "ig", sx, up$age, up$ig), 2))
)

# Row order must not change anyone's result
res_rev <- calc_upload(up[nrow(up):1, ])
res_rev <- res_rev[order(res_rev$ID), ]
stopifnot(near(res_rev$avacc_z, res$avacc_z), near(res_rev$ig_z, res$ig_z),
          near(res_rev$avacc_perc_pred, res$avacc_perc_pred), near(res_rev$ig_perc_pred, res$ig_perc_pred))

# "both" is not a valid sex for individual-level uploads
bad <- up; bad$sex[2] <- "both"
msg <- tryCatch({ calc_upload(bad); "no error" }, error = function(e) conditionMessage(e))
stopifnot(grepl("Sex column", msg))

#------------------------------------------------------------------------------------------
# 2. Cohort-level data (summarised), non-stratified: uses the pooled (_b) models
#------------------------------------------------------------------------------------------

res_b <- run_server(quote({
  session$setInputs(age_b = 50, avacc_b = 32, ig_b = -2.45, height_b = 170, weight_b = 70, Calculate_b = 1)
  refplot_b() # must build without error
  list(perc = percentile_results_b(), med = calc_50_perc_b())
}))
stopifnot(
  near(res_b$perc$percentile, round(pnorm(c(z("avacc", "b", 50, 32), z("ig", "b", 50, -2.45))), 3) * 100),
  near(res_b$med$perc50, c(med("avacc", "b", 50), med("ig", "b", 50)), tol = 1e-6)
)

#------------------------------------------------------------------------------------------
# 3. Cohort-level data (summarised), stratified: female VO2max delta uses the female row
#------------------------------------------------------------------------------------------

cvd_g <- run_server(quote({
  session$setInputs(age_m = 50, age_f = 50, avacc_m = 35, avacc_f = 25, ig_m = -2.4, ig_f = -2.6,
                    height_m = 180, height_f = 165, weight_m = 80, weight_f = 60, Calculate_g = 1)
  datasetIncrease_0_g_cvd()
}))
pf <- data.frame(Age = 50, Sex = 1, BMI = 60 / 1.65^2, ig_gradient_pla = -2.6, ACC_day_mg_pla = 25)
inc_f <- interpretablePA:::find_delta_cvd(ml$mod, pf, fix = c("Age", "Sex", "BMI", "ig_gradient_pla"), delta_y_abs = 3.5)
stopifnot(grepl(paste0("Females: Average acceleration: ", round(inc_f - 25, 1), ","), cvd_g, fixed = TRUE))

#------------------------------------------------------------------------------------------
# 4. Individual-level data: BMI uses height in cm; percentiles unchanged
#------------------------------------------------------------------------------------------

res_i <- run_server(quote({
  session$setInputs(sex_i = "f", age_i = 50, height_i = 175, weight_i = 70, avacc_i = 29.72, ig_i = -2.549, Calculate_i = 1)
  list(perc = percentile_results_i(), inc = datasetIncrease())
}))
pf_i <- data.frame(Age = 50, Sex = 1, BMI = 70 / 1.75^2, ig_gradient_pla = -2.549, ACC_day_mg_pla = 29.72)
inc_acc <- interpretablePA:::find_delta_x(ml$mod, pf_i, fix = c("Age", "Sex", "BMI", "ig_gradient_pla"), delta_y_perc = 0.035)
stopifnot(
  near(res_i$perc$percentile, round(pnorm(c(z("avacc", "f", 50, 29.72), z("ig", "f", 50, -2.549))), 3) * 100),
  near(res_i$inc[1], inc_acc - 29.72, tol = 1e-3)
)

cat("All interpret.pa() checks passed.\n")
