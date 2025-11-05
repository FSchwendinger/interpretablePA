## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example code, eval=FALSE-------------------------------------------------
# library(interpretablePA)
# 
# # Run with only part2 file (subject data in part2)
# interpretablePA::interpret.pa.centiles(
#   part2_path = "data/part2_summary.csv",
#   reference_set = "nhanes"
# )
# 
# # Run with external subject data
# interpretablePA::interpret.pa.centiles(
#   dat_path = "data/subject_data.csv",
#   part2_path = "data/part2_summary.csv",
#   reference_set = "rowlands",
#   col_avacc = "custom_avacc_column",
#   col_ig = "custom_ig_column"
# )
# 
# # Full example call
# interpretablePA::interpret.pa.centiles(
#   dat_path =  "PATH_TO_FILE/participant_characteristics.xlsx",
#   part2_path = "PATH_TO_GGIR_PART2_FILE/part2_summary.csv",
#   output_path = "PATH_TO_OUTPUT_DIR/",
#   col_id = "ID",
#   col_sex = "sex",
#   col_age = "age",
#   sex_code_male = "0",
#   sex_code_female = "1",
#   reference_set = "rowlands",
#   col_avacc = "AD_mean_ENMO_mg_0.24hr",
#   col_ig = "AD_ig_gradient_ENMO_0.24hr"
# )

## ----example simulated data, eval = FALSE, warning=FALSE, error=FALSE---------
# 
# 
# set.seed(123)
# 
# # Create a temporary working directory
# tdir <- tempdir()
# csv_path <- file.path(tdir, "sim_part2_summary.csv")
# 
# # Simulate a small dataset
# sim_data <- data.frame(
#   ID = sprintf("P%03d", 1:10),
#   sex = sample(c("0", "1"), 10, replace = TRUE),
#   age = sample(20:60, 10, replace = TRUE),
#   AD_mean_ENMO_mg_0.24hr = runif(10, 20, 40),
#   AD_ig_gradient_ENMO_0.24hr = runif(10, -2.9, -2.3)
# )
# 
# # Save simulated data
# write.csv(sim_data, csv_path, row.names = FALSE)
# 
# # Ensure subfolder for plots exists (named after basename without extension)
# dir.create(file.path(tdir, "sim_part2_summary"), showWarnings = FALSE)
# 
# # Run the function with simulated data
# results <- interpretablePA::interpret.pa.centiles(
#   part2_path = csv_path,
#   col_id = "ID",
#   col_sex = "sex",
#   col_age = "age",
#   sex_code_male = "0",
#   sex_code_female = "1",
#   reference_set = "nhanes",
#   col_avacc = "AD_mean_ENMO_mg_0.24hr",
#   col_ig = "AD_ig_gradient_ENMO_0.24hr"
# )
# 
# # Show output
# head(results)
# 

## ----reference to documentation, eval = FALSE---------------------------------
# ?interpret.pa.centiles

