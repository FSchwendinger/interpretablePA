# app.R – launcher for standalone execution
if (!requireNamespace("interpretablePA", quietly = TRUE)) {
  stop("Please install the interpretablePA package first.")
}

# Launch the Shiny app
interpretablePA::interpret.pa()
