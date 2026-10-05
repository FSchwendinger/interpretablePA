# Checks for interpret.pa.centiles(), using synthetic GGIR part 2 and subject files in tempdir()
# Run with the package installed: Rscript tests/test-interpret-pa-centiles.R

suppressPackageStartupMessages(library(interpretablePA))

out_dir <- tempfile("ipa_centiles_")
dir.create(out_dir)

# Run quietly; return the result plus any warnings
run <- function(...) {
  warnings <- character(0)
  res <- withCallingHandlers(
    suppressMessages({ invisible(capture.output(r <- interpret.pa.centiles(..., output_path = out_dir))); r }),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  list(res = res, warnings = warnings)
}
centile <- function(res, id, metric) res[[paste0(metric, "_centile")]][res$ID == id]

# Rowlands (40-80 y). Men at age 40: AvAcc median 31.35, IG median -2.393; women: 33.07, -2.451
part2 <- data.frame(
  ID = c("A", "B", "C", "D", "F"),
  sex = c(0, 1, 0, 1, 0),
  age = c(40, 40, 60, 21, 40.5),
  AD_mean_ENMO_mg_0.24hr = c(31.35, 33.07, 5, 30, (31.35 + 31.24) / 2),   # F: halfway between the age-40 and age-41 medians
  AD_ig_gradient_ENMO_0.24hr = c(-2.393, -2.451, NA, -2.5, (-2.393 - 2.397) / 2)
)
part2_path <- file.path(out_dir, "part2_summary.csv")
write.csv(part2, part2_path, row.names = FALSE)

#------------------------------------------------------------------------------------------
# 1. Part 2 file only
#------------------------------------------------------------------------------------------

r <- run(part2_path = part2_path, reference_set = "rowlands")$res
stopifnot(
  centile(r, "A", "avacc") == "50", centile(r, "A", "ig") == "50",       # men's IG uses the IG table
  centile(r, "B", "avacc") == "50", centile(r, "B", "ig") == "50",
  centile(r, "C", "avacc") == "below 3rd percentile",                    # ordinal label
  is.na(centile(r, "C", "ig")),                                          # missing IG does not block AvAcc
  centile(r, "D", "avacc") == "age out of range",
  centile(r, "F", "avacc") == "50", centile(r, "F", "ig") == "50"        # interpolated between ages 40 and 41
)

# Results are written to a new run folder inside output_path
run_dir <- list.files(out_dir, pattern = "^interpretablePA_centiles_rowlands_", full.names = TRUE)
stopifnot(length(run_dir) == 1,
          all(c("centile_results.csv", "avacc_centile_distribution.png", "ig_centile_distribution.png",
                "avacc_centile_vs_age.png", "ig_centile_vs_age.png") %in% list.files(run_dir)))

#------------------------------------------------------------------------------------------
# 2. Nothing to plot (adults with the children's reference): no crash, CSV still written
#------------------------------------------------------------------------------------------

r2 <- run(part2_path = part2_path, reference_set = "fairclough")$res
stopifnot(all(r2$avacc_centile == "age out of range" | is.na(r2$avacc_centile)))
run_dir2 <- list.files(out_dir, pattern = "^interpretablePA_centiles_fairclough_", full.names = TRUE)
stopifnot(file.exists(file.path(run_dir2, "centile_results.csv")), !any(grepl("\\.png$", list.files(run_dir2))))

#------------------------------------------------------------------------------------------
# 3. Separate subject file: custom ID column, untrimmed IDs, sex codes in other case, unmatched IDs
#------------------------------------------------------------------------------------------

subj <- data.frame(participant = c(" A", "B", "Z"), gender = c("M", "f", "x"), years = c(40, 40, 50))
subj_path <- file.path(out_dir, "subjects.csv")
write.csv(subj, subj_path, row.names = FALSE)

out <- run(dat_path = subj_path, part2_path = part2_path, reference_set = "rowlands",
           col_id = "participant", col_sex = "gender", col_age = "years", sex_code_male = "m", sex_code_female = "F")
r3 <- out$res
stopifnot(
  identical(sort(r3$ID), c("A", "B", "Z")),                              # Z kept although it has no GGIR data
  centile(r3, "A", "ig") == "50", centile(r3, "B", "avacc") == "50",
  is.na(r3$avacc[r3$ID == "Z"]),
  any(grepl("no GGIR data", out$warnings)),                              # Z
  any(grepl("no subject data", out$warnings)),                           # C, D, F
  any(grepl("set to NA: x", out$warnings))                               # unmatched sex code
)

#------------------------------------------------------------------------------------------
# 4. NHANES: men and women are compared with different AvAcc tables
#------------------------------------------------------------------------------------------

nh <- data.frame(ID = c("M", "W"), sex = c(0, 1), age = c(20, 20),
                 AD_mean_ENMO_mg_0.24hr = c(36, 36), AD_ig_gradient_ENMO_0.24hr = c(-2.5, -2.5))
nh_path <- file.path(out_dir, "nhanes_part2.csv")
write.csv(nh, nh_path, row.names = FALSE)
r4 <- run(part2_path = nh_path, reference_set = "nhanes")$res
stopifnot(centile(r4, "M", "avacc") != centile(r4, "W", "avacc"))

#------------------------------------------------------------------------------------------
# 5. Rows without an ID are left out instead of being matched to each other
#------------------------------------------------------------------------------------------

p2_noid <- data.frame(ID = c("A", "", NA), sex = c(0, 1, 1), age = c(40, 40, 40),
                      AD_mean_ENMO_mg_0.24hr = c(31.35, 50, 60), AD_ig_gradient_ENMO_0.24hr = c(-2.393, -2.2, -2.1))
subj_noid <- data.frame(ID = c("A", "", NA), sex = c(0, 1, 1), age = c(40, 41, 42))
p2_noid_path <- file.path(out_dir, "part2_noid.csv")
subj_noid_path <- file.path(out_dir, "subjects_noid.csv")
write.csv(p2_noid, p2_noid_path, row.names = FALSE)
write.csv(subj_noid, subj_noid_path, row.names = FALSE)

out5 <- run(dat_path = subj_noid_path, part2_path = p2_noid_path, reference_set = "rowlands")
stopifnot(
  identical(out5$res$ID, "A"), out5$res$avacc == 31.35,
  any(grepl("without an ID .*2 in the subject file, 2 in the part 2 file", out5$warnings))
)

#------------------------------------------------------------------------------------------
# 6. A run never reuses an existing run folder (e.g. two runs in the same second)
#------------------------------------------------------------------------------------------

coll_dir <- tempfile("ipa_collision_")
dir.create(coll_dir)
# Occupy the folder names of the next seconds, so the run below must avoid an existing name
existing <- paste0("interpretablePA_centiles_rowlands_", format(Sys.time() + 0:10, "%Y-%m-%d_%H%M%S"))
for (d in existing) {
  dir.create(file.path(coll_dir, d))
  writeLines("previous run", file.path(coll_dir, d, "centile_results.csv"))
}
invisible(capture.output(suppressWarnings(suppressMessages(
  interpret.pa.centiles(part2_path = part2_path, output_path = coll_dir, reference_set = "rowlands")))))
new_dir <- setdiff(list.files(coll_dir), existing)
stopifnot(
  length(new_dir) == 1, grepl("_2$", new_dir),
  file.exists(file.path(coll_dir, new_dir, "centile_results.csv")),
  all(sapply(existing, function(d) identical(readLines(file.path(coll_dir, d, "centile_results.csv")), "previous run")))
)

cat("All interpret.pa.centiles() checks passed.\n")
