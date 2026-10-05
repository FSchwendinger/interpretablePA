# Checks for the downloadable report of interpret.pa(): runs the real Shiny server via
# shiny::testServer() for all four input modes, checks the collected report data and,
# if pandoc is available, renders the HTML report.
# Run with the package installed: Rscript tests/test-report.R

suppressPackageStartupMessages(library(interpretablePA))

ml   <- interpretablePA:::model_list
med  <- function(metric, sex, age) centiles.pred(ml[[paste0("centile_", metric, "_", sex)]], type = "centiles", xname = "Age", xvalues = age, cent = 50, calibration = FALSE)$`50`
near <- function(a, b) isTRUE(all(abs(a - b) <= 0.5))
can_render <- rmarkdown::pandoc_available()

# Run expr inside the app's server and return its value
run_server <- function(expr) {
  out <- new.env()
  shiny::testServer(interpret.pa(), { out$val <- eval(expr) })
  out$val
}

# Goal 1 minutes as in tab 3: reach the 50th percentile, or +5% if already at or above it
goal1_minutes <- function(avacc, median, below_median, activity_mg) {
  round(1440 * (if (below_median) abs(avacc - median) else 0.05 * avacc) / (activity_mg - avacc))
}

#------------------------------------------------------------------------------------------
# 1. Individual-level data
#------------------------------------------------------------------------------------------

r1 <- run_server(quote({
  session$setInputs(sex_i = "f", age_i = 50, height_i = 175, weight_i = 70, avacc_i = 25, ig_i = -2.6,
                    custom_acc = 900, activities = c("2", "4"), report_title = "Participant 017",
                    report_notes = "Baseline <visit>", Calculate_i = 1)
  list(data = report_data(), html = if (can_render) paste(readLines(output$report, warn = FALSE), collapse = "\n"))
}))
d1 <- r1$data
stopifnot(
  d1$mode == "individual", d1$title == "Participant 017",
  nrow(d1$goal1) == 6, nrow(d1$goal3) == 5, nrow(d1$combined) == 2,
  near(d1$goal1$Minutes[1], goal1_minutes(25, med("avacc", "f", 50), TRUE, 80)),     # slow walking
  near(d1$goal3$Minutes[2], round(1440 / (175 - 25))),                               # brisk walking, 1 mg
  grepl("^Average acceleration", d1$goal2)
)

#------------------------------------------------------------------------------------------
# 2. Cohort-level data, stratified: men below and women above their 50th percentile
#------------------------------------------------------------------------------------------

r2 <- run_server(quote({
  session$setInputs(age_m = 50, age_f = 50, height_m = 180, height_f = 165, weight_m = 80, weight_f = 60,
                    avacc_m = 25, avacc_f = 40, ig_m = -2.6, ig_f = -2.4, custom_acc = 900, Calculate_g = 1)
  list(data = report_data(), cvd_m_brisk = output$output2_m_cvd, html = if (can_render) paste(readLines(output$report, warn = FALSE), collapse = "\n"))
}))
d2 <- r2$data
stopifnot(
  d2$mode == "stratified", is.null(d2$combined),                                     # no two activities ticked
  near(d2$goal1$Men[1], goal1_minutes(25, med("avacc", "m", 50), TRUE, 80)),
  near(d2$goal1$Women[1], goal1_minutes(40, med("avacc", "f", 50), FALSE, 80)),      # women use their own percentile
  near(d2$goal3$Men[2], round(1440 / (175 - 25))),
  grepl(paste0(round(1440 / (175 - 25)), " min"), r2$cvd_m_brisk)                     # tab 3 shows the Goal 3 value
)

#------------------------------------------------------------------------------------------
# 3. Cohort-level data, non-stratified
#------------------------------------------------------------------------------------------

r3 <- run_server(quote({
  session$setInputs(age_b = 50, height_b = 170, weight_b = 70, avacc_b = 32, ig_b = -2.45,
                    custom_acc = 900, Calculate_b = 1)
  list(data = report_data(), html = if (can_render) paste(readLines(output$report, warn = FALSE), collapse = "\n"))
}))
d3 <- r3$data
stopifnot(d3$mode == "non-stratified", near(d3$percentiles$Percentile, c(46.1, 41.9)), grepl("not available", d3$goal2))

#------------------------------------------------------------------------------------------
# 4. Cohort-level data, raw upload
#------------------------------------------------------------------------------------------

up <- data.frame(ID = c("A", "B", "C"), avacc = c(30, 25, 40), ig = c(-2.5, -2.6, -2.3), age = c(25, 40, 55), sex = c("f", "f", "m"))
path <- tempfile(fileext = ".csv")
write.csv(up, path, row.names = FALSE)
r4 <- run_server(bquote({
  session$setInputs(upload = data.frame(name = "u.csv", size = 1, type = "text/csv", datapath = .(path)), Calculate_r = 1)
  list(data = report_data(), html = if (can_render) paste(readLines(output$report, warn = FALSE), collapse = "\n"))
}))
d4 <- r4$data
stopifnot(d4$mode == "raw", nrow(d4$results) == 3, d4$entered$Women == 2,
          all(c("avacc_percentile", "ig_percentile", "avacc_z") %in% names(d4$results)))

#------------------------------------------------------------------------------------------
# 5. Rendered HTML reports
#------------------------------------------------------------------------------------------

if (can_render) {
  html <- list(r1$html, r2$html, r3$html, r4$html)
  stopifnot(
    grepl("Participant 017", html[[1]]), grepl("Baseline &lt;visit&gt;", html[[1]]),   # user text is escaped
    all(sapply(html[1:3], grepl, pattern = "Goal 3: Reduce risk of death and disease")),
    grepl("Results per participant", html[[4]]), !grepl("Goal 1", html[[4]]),
    all(sapply(html, grepl, pattern = "data:image/png;base64")),                        # plot embedded
    all(sapply(html, grepl, pattern = 'class="app-logo"'))                              # package logo
  )
} else {
  cat("pandoc not available: HTML rendering not checked.\n")
}

cat("All report checks passed.\n")
