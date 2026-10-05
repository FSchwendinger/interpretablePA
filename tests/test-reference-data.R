# Checks for the centile reference tables used by interpret.pa.centiles() (model_list2)
# Run with the package installed: Rscript tests/test-reference-data.R

suppressPackageStartupMessages(library(interpretablePA))

ml2  <- interpretablePA:::model_list2
val  <- function(name, age, centile) { m <- ml2[[name]]; m$values[[paste0("x", centile)]][m$ages == age] }
near <- function(a, b) isTRUE(abs(a - b) < 1e-6)

# No two tables are identical (e.g. a copied Excel sheet)
pairs <- combn(names(ml2), 2, simplify = FALSE)
dupes <- Filter(function(p) isTRUE(all.equal(as.matrix(ml2[[p[1]]]$values), as.matrix(ml2[[p[2]]]$values), check.attributes = FALSE)), pairs)
stopifnot(length(dupes) == 0)

# AvAcc tables are positive, IG tables negative
stopifnot(
  all(sapply(ml2[grepl("_avacc_", names(ml2))], function(m) all(m$values > 0))),
  all(sapply(ml2[grepl("_ig_", names(ml2))], function(m) all(m$values < 0)))
)

# Rowlands IG men = Supplementary Table 3 of Rowlands et al. (2025)
stopifnot(near(val("rowlands_centile_ig_m", 40, 50), -2.393), near(val("rowlands_centile_ig_m", 80, 97), -2.300))

# NHANES: corrected AvAcc women; IG men and women in the right tables
stopifnot(
  near(val("nhanes_centile_avacc_m", 20, 50), 38.73), near(val("nhanes_centile_avacc_f", 20, 50), 34.86),
  near(val("nhanes_centile_ig_m", 20, 50), -2.492), near(val("nhanes_centile_ig_f", 20, 50), -2.580)
)

cat("All reference data checks passed.\n")
