# Regression test: interpret.pa() must work when called as interpretablePA::interpret.pa()
# (as shown in the README) without attaching interpretablePA or any of its dependencies.
# The other tests call library(interpretablePA), which attaches the Depends packages and
# would hide missing imports (e.g. 'could not find function "shinytheme"').
# The check therefore runs in a fresh R process started with --vanilla.
# Run with the package installed: Rscript tests/test-namespace-imports.R

child_code <- '
up <- data.frame(ID = 1:2, avacc = c(30, 40), ig = c(-2.5, -2.3), age = c(25, 55), sex = c("f", "m"))
path <- tempfile(fileext = ".csv")
write.csv(up, path, row.names = FALSE)

deps <- paste0("package:", c("interpretablePA", "shiny", "shinythemes", "shinyalert", "shinybusy",
                              "shinyBS", "gamlss", "directlabels", "ggpubr", "ggplot2", "dplyr", "tidyr"))

# 1. Building the app evaluates the UI (shinytheme, fa_i, add_busy_spinner, bsCollapse, ...)
app <- interpretablePA::interpret.pa()
attached <- intersect(search(), deps)
if (length(attached)) stop("Unexpectedly attached: ", paste(attached, collapse = ", "))

# 2. Static check while the search path is still clean: every function the package calls
#    must resolve through its own namespace and imports (covers server code paths too)
problems <- character()
codetools::checkUsageEnv(asNamespace("interpretablePA"), suppressLocalUnused = TRUE,
                         suppressParamAssigns = TRUE, skipWith = TRUE,
                         report = function(x) problems <<- c(problems, x))
problems <- grep("no visible global function definition", problems, value = TRUE)
if (length(problems)) stop("Unresolved functions:\\n", paste(problems, collapse = ""))

# 3. Run real server calculations. Note: shiny::testServer() itself attaches shiny,
#    which is why the checks above run first.
out <- new.env()
shiny::testServer(app, {
  # Upload path: pivot_longer, centiles.pred, z.scores
  session$setInputs(upload = data.frame(name = "u.csv", size = 1, type = "text/csv", datapath = path))
  out$upload <- calc_50_perc_uploaded()
  # Cohort path: shinyalert observer and the reference plot (geom_dl, dl.trans, ggplot2)
  session$setInputs(age_b = 50, avacc_b = 32, ig_b = -2.45, height_b = 170, weight_b = 70, Calculate_b = 1)
  out$plot <- refplot_b()
})

stopifnot(
  nrow(out$upload) == 2,
  all(is.finite(out$upload$avacc_z)), all(is.finite(out$upload$ig_z)),
  !is.null(out$plot)
)
grDevices::pdf(NULL); print(out$plot); invisible(grDevices::dev.off())
cat("namespace-imports child: OK\\n")
'

script <- tempfile(fileext = ".R")
writeLines(child_code, script)
rscript <- file.path(R.home("bin"), "Rscript")
# Pass on the current library paths so the child finds the same installed package
libs <- paste(.libPaths(), collapse = .Platform$path.sep)
out <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(script)),
                                stdout = TRUE, stderr = TRUE, env = paste0("R_LIBS=", shQuote(libs))))
status <- attr(out, "status")
if (!is.null(status) && status != 0) {
  cat(out, sep = "\n")
  stop("interpretablePA::interpret.pa() failed in a fresh R session (see output above)")
}
stopifnot(any(grepl("namespace-imports child: OK", out, fixed = TRUE)))
