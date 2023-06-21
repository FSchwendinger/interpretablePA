#' Function Name: interpret.pa()
#'
#' The interpret.pa() function is a part of an R application that supports the use of cut-point-free metrics, namely Average Acceleration (AvAcc) and Intensity Gradient (IG), assessed by wrist-worn triaxial accelerometers and calculated over the 24-hour day. This function turns abstract accelerometer data into understandable information by classifying the levels of physical activity based on age- and sex-specific reference values and translating the cut-point-free accelerometer metrics into meaningful outcomes.
#' @param None Not necessary
#' @keywords accelerometer human_movement physical_activity
#' @examples
#' interpret.pa()
#' The interpret.pa() function returns a Shiny application.


#==========================================================================================
# Paths
#==========================================================================================

#dat_path <- "./data"

#==========================================================================================
# Helper functions
#==========================================================================================

#source("helpers.R")

#==========================================================================================
# Load packages
#==========================================================================================

# library(tidyverse)
# library(shiny)
# library(palmerpenguins)
# library(shinythemes)
# library(shinyalert)
# library(gamlss)
# library(rms)
# # library(ggplot2)
# library(scales)
# library(directlabels)
# library(ggpubr)
# library(fontawesome)
# library(shinybusy)
# library(shinyjs)
# library(shinyBS)
# library(DT)


#==========================================================================================
# UI
#==========================================================================================

interpret.pa <- function() {
  message(
    "Thank you for using interpretablePA. Please refer to the original study when using this application in publications etc.: \n\n",
    "Schwendinger F., Wagner J., Knaier R., Infanger D., Rowlands A., Hinrichs T., & Schmidt-Trucksaess A. (2023). Reference values for cut-point-free and traditional accelerometer metrics and associations with cardiorespiratory fitness: a cross-sectional study of healthy adults aged 20 to 89 years. medRxiv. doi: https://doi.org/10.1101/2023.04.19.23288786\n"
  )


  # source("R/helpers.R")

  # Load model files (gamlss), store them in a list and name them accordingly
  # mod <- lapply(Sys.glob(path = paste0(dat_path, "/centile_avacc_f.rds")), readRDS)
  # model_list <- lapply(Sys.glob(path = paste0("./data", "/*.rds")), readRDS)
  # names(model_list) <- gsub(".rds", "", grep("/*.rds", list.files(paste0("./data")), value = TRUE))

  ui <-
    shiny::fluidPage(
      titlePanel(title = div(
        img(
          src = "www/app_logo.png",
          width = "10%",
          height = "10%"
        ),
        strong("{interpretablePA}")
      )),
      div(
        h4("Making cut-point-free accelerometer metrics interpretable"),
        style = "color:black"
      ),
      br(),
      theme = shinytheme("flatly"),

      tagList(
        shinyjs::useShinyjs(),
        navbarPage(
          "Navigation",
          collapsible = TRUE,
          tabPanel(
            "Introduction",
            mainPanel(
              h1("Introduction"),
              p(
                "Accelerometers have become valuable tools to assess physical activity in research settings as well as clinical practice.
    Traditional accelerometer metrics that rely on absolute acceleration cut-points have several pitfalls. Alternative accelerometer metrics, namely daily average acceleration (AvAcc) and intensity gradient (IG), are cut-point-free and have been shown to be a viable alternative to traditional metrics.
    In addition, AvAcc and IG are strongly related to various health outcomes (i.e. body fat content, physical functioning, and cardiorespiratory fitness)."
              ),
              p(
                "So far, cut-point-free metrics are difficult to interpret. Adequate reference values and a translation into meaningful outcomes were missing. This is about to change now."
              ),

              tags$hr(style = "border-color: black;"),

              h3("This application"),
              p(
                "This application supports the use of cut-point-free metrics,
    namely AvAcc and IG, assessed by wrist-worn triaxial accelerometers and calculated over the 24 h day."
              ),
              p(
                tags$i(strong("interpretablePA")),
                "will turn abstract numbers into understandable information. This will be achieved by the following two steps:"
              ),
              HTML(
                "<ul><li>classifying the levels of physical activity based on age- and sex-specific reference values</li><li>translating the cut-point-free accelerometer metrics into meaningful outcomes</li><li>Reference values come from a population sample of 463 healthy adults aged 20 to 89 years in Switzerland who wore the GENEActiv (sampling frequency @ 50 Hz) on their non-dominant wrist for up to 14 days (Schwendinger et al., 2023)</li></ul>"
              ),
              br(),
              p(img(
                src = "www/numb_bulb.png",
                height = "100%",
                width = "100%"
              )),
              p(
                HTML(
                  "<font size=1>Photos by <a href=https://unsplash.com/@mbaumi?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText>Mika Baumeister</a> and <a href=https://unsplash.com/@mehedi192?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText>MEHEDI HASAN</a> on Unsplash.
  </font>"
                )
              ),
              tags$hr(style = "border-color: black;"),
              h3("Usage"),
              p(
                tags$i(strong("interpretablePA")),
                " works with data that were processed using the R-package",
                tags$a(href = "https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html#1_Introduction", " GGIR"),
                ". It is important that data are processed in a similar manner."
              ),
              p(
                "To start using ",
                tags$i(strong("interpretablePA")),
                ", move to tab ",
                strong("'1) User data'. "),
                "We provide you with three options for entering data:"
              ),
              HTML(
                "<ul><li>Individual-level data: Creates results based on the data of one individual.</li><li>Cohort-level data (summarised): Yields results based on the means/medians of a cohort.</li><li>Cohort-level data (raw): Yields results based on the raw values of each individual of a sample of size = N.</li></ul>"
              ),
              p(
                "Choose the option that is suitable for your type of data and follow the instructions provided in the respective tabs."
              ),
              p(),

              tags$hr(style = "border-color: black;"),
              h3("Citing", tags$i(strong(
                "interpretablePA"
              )),  "and the underlying data"),
              p(
                "Please refer to the original study when using this application in publications etc.:"
              ),
              p(
                "Schwendinger F., Wagner J., Knaier R., Infanger D., Rowlands A., Hinrichs T., & Schmidt-Trucksaess A. (2023). Reference values for cut-point-free and traditional accelerometer metrics and associations with cardiorespiratory fitness: a cross-sectional study of healthy adults aged 20 to 89 years. medRxiv. doi: https://doi.org/10.1101/2023.04.19.23288786"
              ),
              p(),
              tags$hr(style = "border-color: black;"),
              h3("Contact us"),
              p(
                "This package was created by Fabian Schwendinger and Denis Infanger. Alex Rowlands, Jonathan Wagner, Raphael Knaier, Timo Hinrichs, and Arno Schmidt-Trucksaess contributed to the development."
              ),
              p(
                "If you have queries regarding ",
                tags$i(strong("interpretablePA")),
                "or are interested in contributing, feel free to contact us via e-mail."
              ),
              a(
                actionButton(
                  inputId = "email1",
                  label = "Contact",
                  icon = icon("envelope", lib = "font-awesome"),
                  style = "color: #fff; background-color: #F9812A; border-color: #CC6D00"
                ),
                href = "mailto:fabian.schwendinger@unibas.ch"
              )
              ,
              tags$hr(style = "border-color: black;"),
              tags$a(img(src = "www/Unibas_logo.png", height = 72), href = "https://www.unibas.ch/en.html"),
              HTML("&nbsp"),
              HTML("&nbsp"),
              #last HTML command adds space between images
              tags$a(img(src = "www/DSBG_logo.png", height = 72), href = "https://dsbg.unibas.ch/de/"),
              br(),
              br(),
              br()
            )
          ),

  ################################################################################
  # 1) User data
  ################################################################################

  navbarMenu(
    "1) User data",
    tabPanel(
      title = "Individual-level data",
      sidebarLayout(
        position = "right",
        sidebarPanel(
          h3("Description"),
          p(
            "To start the analyses, enter all required data into the respective fields.
               Choose between individual-level data and cohort-level data. To trigger the analyses, press the",
            tags$span(style = "color:DarkOrange", strong("'Calculate'")),
            "button and switch to tab ",
            strong("'2) View results'. ")
          ),
          p(
            "Make sure to hit the ",
            tags$span(style = "color:DarkOrange", strong("'Reset calculations'")),
            "button before you initiate a new calculation."
          ),
          p(
            tags$span(style = "color:red", strong("!")),
            "Be aware that ",
            tags$i(strong("interpretablePA")),
            "is currently only meant for data of adults between 20 to 90 years of age. Data of e.g. children and adolescents may be implemented in the future. If you are interested in contributing, feel free to contact us (see Introduction panel)."
          )
        ),
        mainPanel(
          h1("Individual-level data"),
          fluidRow(
            column(
              6,
              actionButton(
                "reset",
                "Reset calculations",
                icon = fa_i("fas fa-delete-left", verify_fa = FALSE)
              ),
              br(),
              radioButtons(
                "sex_i",
                label = "Sex:",
                choices = list("Female" = "f", "Male" = "m"),
                selected = "f"
              ),
              numericInput(
                inputId = "age_i",
                label = "Age in years:",
                value = 50,
                min = 20,
                max = 100
              ),
              # Input: numeric entry for height
              numericInput(
                inputId = "height_i",
                label = "Height in cm:",
                value = 175,
                min = 130,
                max = 220
              ),
              # weight
              numericInput(
                inputId = "weight_i",
                label = "Body weight in kg:",
                value = 70,
                min = 30,
                max = 250
              ),
              # AvAcc
              numericInput(
                inputId = "avacc_i",
                label = "Average acceleration in mg per day:",
                value = 29.72
              ),
              # IG
              numericInput(
                inputId = "ig_i",
                label = "Intensity gradient:",
                value = -2.549
              )
            ),
            column(6,
                   div(
                     img(
                       src = "www/individual.png",
                       width = "60%",
                       height = "60%"
                     ),
                     style = "text-align: center;"
                   ))
          ),
          actionButton(
            "Calculate_i",
            "Calculate",
            icon = icon("calculator"),
            onclick = "$(tab2).removeClass('disabled');$(tab3).removeClass('disabled');$(tab4).removeClass('disabled')",
            style = "color: #fff; background-color: #F9812A; border-color: #CC6D00"
          ),
        )

      ),
      tags$hr(style = "border-color: black;"),
      tags$a(img(src = "www/Unibas_logo.png", height = 72), href = "https://www.unibas.ch/en.html"),
      HTML("&nbsp"),
      HTML("&nbsp"),
      #last HTML command adds space between images
      tags$a(img(src = "www/DSBG_logo.png", height = 72), href =
               "https://dsbg.unibas.ch/de/"),
      br()
    ),
    tabPanel(
      title = "Cohort-level data (summarised)",
      sidebarLayout(
        position = "right",
        sidebarPanel(
          h3("Description"),
          p(
            "To start the analyses, enter all required data into the respective fields.
               Choose between individual-level data and cohort-level data. To trigger the analyses, press the",
            tags$span(style = "color:DarkOrange", strong("'Calculate'")),
            "button and switch to tab ",
            strong("'2) View results'. ")
          ),
          p(
            "Make sure to hit the ",
            tags$span(style = "color:DarkOrange", strong("'Reset calculations'")),
            "button before you initiate a new calculation."
          ),
          p(
            tags$span(style = "color:red", strong("!")),
            "Be aware that ",
            tags$i(strong("interpretablePA")),
            "is currently only meant for data of adults between 20 to 90 years of age. Data of e.g. children and adolescents may be implemented in the future. If you are interested in contributing, feel free to contact us (see Introduction panel)."
          )
        ),
        mainPanel(
          h1("Cohort-level data (summarised)"),
          fluidRow(column(
            3,

            radioButtons(
              "stratavail",
              label = "Is data stratified by sex?",
              choices = list("Yes" = 0, "No" = 1),
              selected = 0
            )
          ),
          column(6, div(
            img(
              src = "www/cohort_means.png",
              width = "70%",
              height = "70%"
            ),
            style = "text-align: center;"
          ))),
          conditionalPanel(
            condition = "input.stratavail == 0",
            actionButton(
              "reset1",
              "Reset calculations",
              icon = fa_i("fas fa-delete-left", verify_fa = FALSE)
            ),
            fluidRow(
              column(
                6,
                h2("Females"),
                numericInput(
                  inputId = "age_f",
                  label = "Age in years:",
                  value = 50,
                  min = 20,
                  max = 100
                ),
                # Input: numeric entry for height
                numericInput(
                  inputId = "height_f",
                  label = "Height in cm:",
                  value = 170,
                  min = 130,
                  max = 220
                ),
                # weight
                numericInput(
                  inputId = "weight_f",
                  label = "Body weight in kg:",
                  value = 65,
                  min = 30,
                  max = 250
                ),
                # AvAcc
                numericInput(
                  inputId = "avacc_f",
                  label = "Average acceleration in mg per day:",
                  value = 29.72
                ),
                # IG
                numericInput(
                  inputId = "ig_f",
                  label = "Intensity gradient:",
                  value = -2.549
                )
              ),

              column(
                6,
                h2("Males"),
                numericInput(
                  inputId = "age_m",
                  label = "Age in years:",
                  value = 50,
                  min = 20,
                  max = 100
                ),
                # Input: numeric entry for height
                numericInput(
                  inputId = "height_m",
                  label = "Height in cm:",
                  value = 175,
                  min = 130,
                  max = 220
                ),
                # weight
                numericInput(
                  inputId = "weight_m",
                  label = "Body weight in kg:",
                  value = 70,
                  min = 30,
                  max = 250
                ),
                # AvAcc
                numericInput(
                  inputId = "avacc_m",
                  label = "Average acceleration in mg per day:",
                  value = 30.38
                ),
                # IG
                numericInput(
                  inputId = "ig_m",
                  label = "Intensity gradient:",
                  value = -2.450
                )
              )
            ),
            actionButton(
              "Calculate_g",
              "Calculate",
              icon = icon("calculator"),
              onclick = "$(tab2).removeClass('disabled');$(tab3).removeClass('disabled');$(tab4).removeClass('disabled')",
              style = "color: #fff; background-color: #F9812A; border-color: #CC6D00"
            ),

          ),
          conditionalPanel(
            condition = "input.stratavail == 1",
            actionButton(
              "reset2",
              "Reset calculations",
              icon = fa_i("fas fa-delete-left", verify_fa = FALSE)
            ),
            fluidRow(
              column(
                6,
                h2("Non-stratified data (both sexes)"),
                numericInput(
                  inputId = "age_b",
                  label = "Age in years:",
                  value = 50,
                  min = 20,
                  max = 100
                ),
                # Input: numeric entry for height
                numericInput(
                  inputId = "height_b",
                  label = "Height in cm:",
                  value = 175,
                  min = 130,
                  max = 220
                ),
                # weight
                numericInput(
                  inputId = "weight_b",
                  label = "Body weight in kg:",
                  value = 70,
                  min = 30,
                  max = 250
                ),
                # AvAcc
                numericInput(
                  inputId = "avacc_b",
                  label = "Average acceleration in mg per day:",
                  value = 30.38
                ),
                # IG
                numericInput(
                  inputId = "ig_b",
                  label = "Intensity gradient:",
                  value = -2.450
                )
              )
            ),
            actionButton(
              "Calculate_b",
              "Calculate",
              icon = icon("calculator"),
              onclick = "$(tab2).removeClass('disabled');$(tab3).removeClass('disabled');$(tab4).removeClass('disabled')",
              style = "color: #fff; background-color: #F9812A; border-color: #CC6D00"
            ),

          )
        )
      ),
      tags$hr(style = "border-color: black;"),
      tags$a(img(src = "www/Unibas_logo.png", height = 72), href = "https://www.unibas.ch/en.html"),
      HTML("&nbsp"),
      HTML("&nbsp"),
      #last HTML command adds space between images
      tags$a(img(src = "www/DSBG_logo.png", height = 72), href =
               "https://dsbg.unibas.ch/de/"),
      br(),
      br(),
      br()
    ),

    tabPanel(
      title = "Cohort-level data (raw)",
      sidebarLayout(
        position = "right",
        sidebarPanel(
          h3("Description"),
          p(
            "
               To trigger the analyses, press the",
            tags$span(style = "color:DarkOrange", strong("'Browse'")),
            "button and select the csv file with your data. Make sure the file does not include empty rows. Missing data should be denoted as 'NA'. The file needs to be comma-separated. Do not use semicolons. Sex needs to be coded as follows: males = m, females = f."
          ),
          p(
            "To upload a different file, press the ",
            tags$span(style = "color:DarkOrange", strong("'Clear file'")),
            "button."
          ),
          p(
            tags$span(style = "color:red", strong("!")),
            "Be aware that ",
            tags$i(strong("interpretablePA")),
            "is currently only meant for data of adults between 20 to 90 years of age. Data of e.g. children and adolescents may be implemented in the future. If you are interested in contributing, feel free to contact us (see Introduction panel)."
          ),
          p(
            "To create percentile graphs of average acceleration and intensity gradient with all your data plotted, press the",
            tags$span(style = "color:orange", strong("'Create graphs'")),
            " button and go to the ",
            strong("'2) View results' "),
            "panel to inspect them."
          ),
          p(
            "Download the updated file via the ",
            tags$span(style = "color:DarkOrange", strong("'Download results'")),
            "button."
          ),
          br(),
          h4("Code book"),
          HTML(
            "<ul><li>avacc = average acceleration in mg,</li><li>ig = intensity gradient (AD_24h),</li><li>cent50_avacc = Value of AvAcc corresponding to the 50th percentile (same age and sex),</li><li>cent50_ig = Value of IG corresponding to the 50th percentile (same age and sex),</li><li>ig_perc_pred = IG as percent predicted,</li><li>avacc_perc_pred = AvAcc as percentage predicted,</li><li>avacc_z = Corresponding z-score,</li><li>ig_z = Corresponding z-score.</li></ul>"
          ),
          br(),
        ),

        mainPanel(h1("Cohort-level data (raw)"),
                  fluidRow(
                    column(
                      6,
                      p(
                        "To start, download the template file by clicking on the button below and fill in your data. The column names in the file must not be changed."
                      ),
                      br(),
                      downloadButton("download_template", "Download template"),
                      p(),
                      tags$hr(style = "border-color: black;"),
                      br(),
                      fileInput(
                        inputId = "upload",
                        label = "Upload filled-in template file",
                        accept = ".csv"
                      ),
                      tags$hr(style = "border-color: black;"),
                      br(),
                      h4("Preview of your upload"),
                      DT::dataTableOutput("usertemp"),
                      br(),

                      fluidRow(
                        column(width = 4,
                               actionButton(
                                 "clear",
                                 "Clear file",
                                 icon = fa_i("fas fa-delete-left", verify_fa = FALSE)
                               )),
                        column(
                          width = 4,
                          actionButton(
                            "Calculate_r",
                            "Create graphs",
                            icon = icon("calculator"),
                            onclick = "$(tab2).removeClass('disabled')",
                            style = "color: #fff; background-color: #F9812A; border-color: #CC6D00"
                          )
                        )
                      ),
                      tags$hr(style = "border-color: black;"),


                      add_busy_spinner(spin = "fading-circle", position = "full-page"),
                      br(),
                      h4("Preview of your results and download"),
                      p(
                        "Below, you can download the previously uploaded file with additional columns that include AvAcc and IG as percentage of predicted (compared to our reference values) as well as corresponding z-scores."
                      ),
                      downloadButton("download_perc_pred", "Download results", style =
                                       "color: #fff; background-color: #F9812A; border-color: #CC6D00"),
                      br(),
                      br(),
                      div(style = 'overflow-x: scroll', DT::dataTableOutput("modifiedData")),
                      br(),
                      plotOutput("plot_r", height = "800px")
                    ),
                    column(6,

                           div(
                             img(
                               src = "www/cohort_raw.png",
                               width = "70%",
                               height = "70%"
                             ),
                             style = "text-align: center;"
                           ))
                  ))
      ),
      tags$hr(style = "border-color: black;"),
      tags$a(img(src = "www/Unibas_logo.png", height = 72), href = "https://www.unibas.ch/en.html"),
      HTML("&nbsp"),
      HTML("&nbsp"),
      #last HTML command adds space between images
      tags$a(img(src = "www/DSBG_logo.png", height = 72), href =
               "https://dsbg.unibas.ch/de/"),
      br(),
      br(),
      br()
    )
  ),

  ################################################################################
  # 2) View results
  ################################################################################

  tabPanel(
    "2) View results",
    value = "2) View results",
    tags$script(
      '
    var tab2 = $(\'a[data-value="2) View results"]\').parent().addClass("disabled");
    $(function(){
      $(tab2.parent()).on("click", "li.disabled", function(e) {
        e.preventDefault();
        return false;
      });
    });
    '
    ),

    sidebarLayout(
      position = "right",
      sidebarPanel(
        h3("Entered data"),
        p(
          "Once the required data were provided in ",
          strong("tab '1) User data'"),
          ", the entered values of average acceleration and intensity gradient are displayed right below in this box."
        ),
        div(id = "entered_avacc", textOutput("entered_avacc")),
        div(id = "entered_avacc_f", textOutput("entered_avacc_f")),
        div(id = "entered_ig", textOutput("entered_ig")),
        div(id = "entered_ig_f", textOutput("entered_ig_f")),
        div(id = "entered_avacc_m", textOutput("entered_avacc_m")),
        div(id = "entered_ig_m", textOutput("entered_ig_m")),
        div(id = "entered_avacc_b", textOutput("entered_avacc_b")),
        div(id = "entered_ig_b", textOutput("entered_ig_b"))
        ,
        p(),
        h3("Interpretation"),
        h4("Percentile curves"),
        p(
          "The figures show normative data for average acceleration and intensity gradient for healthy individuals.
                                                                                                                                                                                                                              The ",
          tags$span(style = "color:green", "green"),
          " dots in the figures illustrate where the individual or cohort is located compared to healthy individuals of the same age."
        ),
        h4("Calculated percentiles"),
        p(
          "This section provides the exact percentile at which the individual or cohort is located compared to healthy age-matched counterparts."
        ),
        br(),
        downloadButton("download_plot", "Download plots", style = "color: #fff; background-color: #F9812A; border-color: #CC6D00")
      ),
      mainPanel(
        h1("View results"),
        add_busy_spinner(spin = "fading-circle", position = "full-page"),
        verbatimTextOutput('contents'),
        verbatimTextOutput('contents_g'),
        verbatimTextOutput('contents_b'),
        plotOutput("plot1", height = "800px"),
        br(),
        div(id = "perc_50_avacc",
            verbatimTextOutput('perc_50_avacc')),
        div(id = "perc_50_avacc_b",
            verbatimTextOutput('perc_50_avacc_b')),
        div(id = "perc_50_avacc_g",
            verbatimTextOutput('perc_50_avacc_g')),
        br(),
        div(id = "perc_50_ig",
            verbatimTextOutput('perc_50_ig')),
        div(id = "perc_50_ig_b",
            verbatimTextOutput('perc_50_ig_b')),
        div(id = "perc_50_ig_g",
            verbatimTextOutput('perc_50_ig_g')),
        tags$hr(style = "border-color: black;"),
        img(src = "www/Unibas_logo.png", height = 72),
        img(src = "www/DSBG_logo.png", height = 72),
        br(),
        br(),
        br()
      )
    )
  ),

  ################################################################################
  # 3) Translation of results
  ################################################################################

  tabPanel(
    "3) Translation of results",
    value = "3) Translation of results",
    tags$script(
      '
    var tab3 = $(\'a[data-value="3) Translation of results"]\').parent().addClass("disabled");
    $(function(){
      $(tab3.parent()).on("click", "li.disabled", function(e) {
        e.preventDefault();
        return false;
      });
    });
    '
    ),

    mainPanel(
      h1("Translation of results"),
      add_busy_spinner(spin = "fading-circle", position = "full-page"),
      bsCollapse(
        id = "coll_translation",
        open = "Panel 1",
        bsCollapsePanel(
          "Panel 1",
          tags$label(h3('Goal 1: Reaching the 50th percentile')),
          # Status/Output Text Box
          p(HTML(
            paste0(
              "In this panel, we demonstrate how much time spent in different types of activity is needed to reach the 50th percentile (calculations are based on average acceleration). Any of the following activities is sufficient. Based on the strong and independent predictive value of intensity gradient for ",
              "VO",
              tags$sub("2max"),
              ", activities demanding higher intensities are to be preferred. In case the current activity level is above the 50th percentile, the recommendations refer to a 5% improvement in average acceleration."
            )
          )),
          p(h4(
            "Minutes of slow walking (3 kph; 80 m", tags$i("g"), "): "
          )),
          verbatimTextOutput("output1"),
          verbatimTextOutput("output1_b"),
          verbatimTextOutput("output1_m"),
          verbatimTextOutput("output1_f"),
          p(h4(
            "Minutes of brisk walking (5 kph; 175 m", tags$i("g"), "): "
          )),
          verbatimTextOutput("output2"),
          verbatimTextOutput("output2_b"),
          verbatimTextOutput("output2_m"),
          verbatimTextOutput("output2_f"),
          p(h4(
            "Minutes of fast walking (6.5 kph; 400 m", tags$i("g"), "): "
          )),
          verbatimTextOutput("output3"),
          verbatimTextOutput("output3_b"),
          verbatimTextOutput("output3_m"),
          verbatimTextOutput("output3_f"),
          p(h4(
            "Minutes of slow running (8 kph; 750 m", tags$i("g"), "): "
          )),
          verbatimTextOutput("output4"),
          verbatimTextOutput("output4_b"),
          verbatimTextOutput("output4_m"),
          verbatimTextOutput("output4_f"),
          p(h4(
            "Minutes of moderate running (10 kph; 1000 m", tags$i("g"), "): "
          )),
          verbatimTextOutput("output5"),
          verbatimTextOutput("output5_b"),
          verbatimTextOutput("output5_m"),
          verbatimTextOutput("output5_f"),
          p(h4(
            "Customised input of acceleration that should be replaced: "
          )),
          numericInput(
            inputId = "custom_acc",
            label = "Acceleration of activity:",
            value = 900
          ),
          verbatimTextOutput("output_cust"),
          verbatimTextOutput("output_cust_b"),
          verbatimTextOutput("output_cust_m"),
          verbatimTextOutput("output_cust_f"),

          br(),
          p(h3("Combined recommendations: ")),
          p(
            "Choose two types of activity that are suitable for you. The necessary improvement in average acceleration to reach the 50th percentile or a 5% improvement (if already above the 50th percentile) will to equal parts be allocated to the two selected activities."
          ),
          checkboxGroupInput(
            "activities",
            "Variables:",
            c(
              "Slow walking (3 kph)" = "1",
              "Brisk walking (5 kph)" = "2",
              "Fast walking (6.5 kph)" = "3",
              "Slow running (8 kph)" = "4",
              "Moderate running (10 kph)" = "5"
            )
          ),
          #p("With the slider, the fraction of intensive physical activity can be adjusted. A higher number on the slider will lead to an activity recommendation
          # that favours physical activity at higher intensity. A lower number on the slider will base the activity recommendation more on low/moderate intensity physical activity."),
          #sliderInput("intensity", "Intensity fraction", min=0, max=100, value = 70, step = 5, post = "%"),
          br(),
          verbatimTextOutput("output10"),
          verbatimTextOutput("output11"),
          verbatimTextOutput("output12"),
          verbatimTextOutput("output13"),
          verbatimTextOutput("output14"),
          verbatimTextOutput("output10_b"),
          verbatimTextOutput("output11_b"),
          verbatimTextOutput("output12_b"),
          verbatimTextOutput("output13_b"),
          verbatimTextOutput("output14_b"),
          verbatimTextOutput("output10_f"),
          verbatimTextOutput("output11_f"),
          verbatimTextOutput("output12_f"),
          verbatimTextOutput("output13_f"),
          verbatimTextOutput("output14_f"),
          verbatimTextOutput("output10_m"),
          verbatimTextOutput("output11_m"),
          verbatimTextOutput("output12_m"),
          verbatimTextOutput("output13_m"),
          verbatimTextOutput("output14_m"),

          # verbatimTextOutput("Selected"),
          style = "primary"
        )
      ),
      br(),
      bsCollapsePanel(
        "Panel 2",
        p(
          h3(
            "Goal 2: Clinically relevant improvement in cardiorespiratory fitness"
          )
        ),
        p(HTML(
          paste0(
            "This panel shows the necessary absolute change in average acceleration or intensity gradient that corresponds to a clinically relevant improvement in cardiorespiratory fitness measured as ",
            "VO",
            tags$sub("2max"),
            " (1 mL/kg/min)."
          )
        )),
        verbatimTextOutput("inc_abs"),
        p(HTML(
          paste0(
            "Importantly, increasing physical activity levels by the abovementioned quantity will unlikely lead to direct improvements in ",
            "VO",
            tags$sub("2max"),
            ". For this change to occur, prolonged behavioural change is required. These calculations are based on the models presented in Figure 3 in Schwendinger et al. (2023)."
          )
        )),
        style = "primary",
        p(HTML(
          paste0(
            "Reference:<br>
                             Ekblom-Bak E, Ekblom  B, Söderling J, et al. Sex- and age-specific associations between cardiorespiratory fitness, CVD morbidity and all-cause mortality in 266.109 adults.
                              Prev Med 2019;127:105799.
                              "
          )
        ))
      ),
      br(),
      bsCollapsePanel(
        "Panel 3",
        p(h3("Goal 3: Reduce risk of death and disease")),
        p(HTML(
          paste0(
            "Any of the below activities undertaken daily for the specified duration may correspond to a 5.2% lower all-cause mortality. These calculations are based on the association of average acceleration with all-cause mortality in inactive adults."
          )
        )),
        verbatimTextOutput("output1_cvd"),
        verbatimTextOutput("output1_m_cvd"),
        verbatimTextOutput("output1_f_cvd"),
        verbatimTextOutput("output1_b_cvd"),

        verbatimTextOutput("output2_cvd"),
        verbatimTextOutput("output2_m_cvd"),
        verbatimTextOutput("output2_f_cvd"),
        verbatimTextOutput("output2_b_cvd"),

        verbatimTextOutput("output3_cvd"),
        verbatimTextOutput("output3_m_cvd"),
        verbatimTextOutput("output3_f_cvd"),
        verbatimTextOutput("output3_b_cvd"),

        verbatimTextOutput("output4_cvd"),
        verbatimTextOutput("output4_m_cvd"),
        verbatimTextOutput("output4_f_cvd"),
        verbatimTextOutput("output4_b_cvd"),

        verbatimTextOutput("output5_cvd"),
        verbatimTextOutput("output5_m_cvd"),
        verbatimTextOutput("output5_f_cvd"),
        verbatimTextOutput("output5_b_cvd"),


        p(HTML(
          paste0(
            "Reference:<br>
                              Rowlands A, Davies M, Dempsey P, et al. Wrist-worn accelerometers: recommending ~1.0 m<i>g</i> as the minimum clinically important difference (MCID) in daily average acceleration for inactive adults.
                              BJSM 2021;55:814-815.
                              "
          )
        )),
        style = "primary"
      ),
      tags$hr(style = "border-color: black;"),
      img(src = "www/Unibas_logo.png", height = 72),
      img(src = "www/DSBG_logo.png", height = 72),
      br(),
      br(),
      br()
    )
  ),

  ################################################################################
  # 4) Download report
  ################################################################################

  tabPanel(
    "4) Download report",
    value = "4) Download report",
    tags$script(
      '
    var tab4 = $(\'a[data-value="4) Download report"]\').parent().addClass("disabled");
    $(function(){
      $(tab4.parent()).on("click", "li.disabled", function(e) {
        e.preventDefault();
        return false;
      });
    });
    '
    ),

    mainPanel(
      h1("Download your report"),
      p(
        img(
          src = "www/work-in-progress.png",
          height = "30%",
          width = "30%"
        )
      ),
      p(
        HTML("<font size=1>Icon created by Freepik on flaticon.com.
  </font>")
      ),
      p(
        "This is work in progress. A function that enables downloading reports will be available in the next version of",
        tags$i(strong("interpretablePA")),
        ".",
        "Stay tuned."
      ),
      add_busy_spinner(spin = "fading-circle", position = "full-page"),
      br(),
      # Define an action button that triggers the download of the summary PDF
      #actionButton("final_report", "Download report", icon=icon("download"), style="color: #fff; background-color: #F9812A; border-color: #CC6D00"), # activate once the report download works
      tags$hr(style = "border-color: black;"),
      img(src = "www/Unibas_logo.png", height = 72),
      img(src = "www/DSBG_logo.png", height = 72)
    ),
    br(),
    br(),
    br()
  )
        )))


#==========================================================================================
# Server
#==========================================================================================

server <- function(input, output, session) {



  #------------------------------------------------------------------------------------------
  # Access the input when user clicks button
  #------------------------------------------------------------------------------------------

  age_i <- eventReactive(input$Calculate_i, {
    input$age_i
  })

  sex_i <- eventReactive(input$Calculate_i, {
    input$sex_i
  })

  height_i <- eventReactive(input$Calculate_i, {
    input$height_i
  })

  weight_i <- eventReactive(input$Calculate_i, {
    input$weight_i
  })

  ig_i <- eventReactive(input$Calculate_i, {
    input$ig_i
  })

  avacc_i <- eventReactive(input$Calculate_i, {
    input$avacc_i
  })


  # For stratified cohort-level data
  # For females
  age_f <- eventReactive(input$Calculate_g, {
    input$age_f
  })

  height_f <- eventReactive(input$Calculate_g, {
    input$height_f
  })

  weight_f <- eventReactive(input$Calculate_g, {
    input$weight_f
  })

  ig_f <- eventReactive(input$Calculate_g, {
    input$ig_f
  })

  avacc_f <- eventReactive(input$Calculate_g, {
    input$avacc_f
  })

  # For males
  age_m <- eventReactive(input$Calculate_g, {
    input$age_m
  })

  height_m <- eventReactive(input$Calculate_g, {
    input$height_m
  })

  weight_m <- eventReactive(input$Calculate_g, {
    input$weight_m
  })

  ig_m <- eventReactive(input$Calculate_g, {
    input$ig_m
  })

  avacc_m <- eventReactive(input$Calculate_g, {
    input$avacc_m
  })

  # For non-stratified cohort-level data

  age_b <- eventReactive(input$Calculate_b, {
    input$age_b
  })

  height_b <- eventReactive(input$Calculate_b, {
    input$height_b
  })

  weight_b <- eventReactive(input$Calculate_b, {
    input$weight_b
  })

  ig_b <- eventReactive(input$Calculate_b, {
    input$ig_b
  })

  avacc_b <- eventReactive(input$Calculate_b, {
    input$avacc_b
  })

  custom_acc <- eventReactive(input$custom_acc, {
    input$custom_acc
  })

  # # For uploaded data
  #
  # sex_u <- eventReactive(input$upload, {
  #   input$sex
  # })
  #
  # age_u <- eventReactive(input$upload, {
  #   input$age
  # })
  #
  # avacc_u <- eventReactive(input$upload, {
  #   input$avacc
  # })
  #
  # ig_u <- eventReactive(input$upload, {
  #   input$ig
  # })



  #------------------------------------------------------------------------------------------
  # Create a downloadable template
  #------------------------------------------------------------------------------------------
output$download_template <- downloadHandler(
  filename = "Template_interpretablePA.csv",
  content = function(file) {
    df <- data.frame(matrix(ncol = 5, nrow=0))
    colnames(df) <- c("ID", "avacc","ig","age","sex")
    write.csv(df, file, col.names= T, row.names = F)
  }
)

  ################################################################################
  # Create models
  ################################################################################

  observeEvent(input$Calculate_i, {
    # Show a modal when the button is pressed
    shinyalert(
      title = "Calculation initiated",
      text = "<h4 style=color:black;>Results are presented in panel <b>2) 'View results'",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#9CE685",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )})


  observeEvent(input$Calculate_g, {
    # Show a modal when the button is pressed
    shinyalert(
      title = "Calculation initiated",
      text = "<h4 style=color:black;>Results are presented in panel <b>2) 'View results'",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#9CE685",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )})


  observeEvent(input$Calculate_b, {
    # Show a modal when the button is pressed
    shinyalert(
      title = "Calculation initiated",
      text = "<h4 style=color:black;>Results are presented in panel <b>2) 'View results'",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#9CE685",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )})


  observeEvent(input$reset, {
    # Show a modal when the button is pressed
    shinyalert(
      title = "Reset successful",
      text = "<h4 style=color:black;>You may now calculate your next result.",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#9CE685",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )})

  observeEvent(input$reset1, {
    # Show a modal when the button is pressed
    shinyalert(
      title = "Reset successful",
      text = "<h4 style=color:black;>You may now calculate your next result.",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#9CE685",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )})

  observeEvent(input$reset2, {
    # Show a modal when the button is pressed
    shinyalert(
      title = "Reset successful",
      text = "<h4 style=color:black;>You may now calculate your next result.",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#9CE685",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )})

  # Create code to upload template with user data


  read_userdata <- eventReactive(input$upload, {
    req(input$upload)

    df <- read.csv(input$upload$datapath, sep = ",", na.strings=c("NA", " "))
    #head(df[0:4,])
  })


  output$usertemp <- DT::renderDataTable({
    DT::datatable(read_userdata(), options = list(dom = 't'))
  })

  observeEvent(input$clear, {
    output$usertemp <- NULL
  })



  ################################################################################
  # Calculations with uploaded file
  ################################################################################

  # Calculate 50th percentile based on data in uploaded file (input$usertemp).
  # Age is in column "age" and sex in column "sex"
  calc_50_perc_uploaded <- eventReactive(input$upload, {

    req(input$upload)

    df <- read_userdata()

    # Make sure that ages are between 20 and 89, inclusive.
    shiny::validate(
      need(all(na.omit(df$age) >= 20) && all(na.omit(df$age) <= 89), "Please enter ages between 20 and 89 (inclusive).")
    )

    #head(df[0:4,])

    # print(df)

    # param_list <- c("avacc", "ig")

    # perc50_frame <- data.frame(
    #   parameter = rep(param_list, times = nrow(df))
    #   , age = rep(df[, "age"], each = 2)
    #   , sex = rep(df[, "sex"], each = 2)
    #   , perc50 = NA
    # )

    df <- df %>%
      mutate(cent50_avacc = NA, cent50_ig = NA)

    for (i in seq_len(nrow(df))) {

      mod_avacc <- model_list[[grep(paste0("centile_", "avacc", "_", df[i, "sex"]), names(model_list))]]
      mod_ig <- model_list[[grep(paste0("centile_", "ig", "_", df[i, "sex"]), names(model_list))]]

      # Which centiles to plot
      # centiles_pred <- c(3, 15, 50, 85, 97)

      # Predict centiles and save them in a data frame
      cent50_avacc <- centiles.pred(
        obj = mod_avacc
        , type = "centiles"
        , xname = "Age"
        , xvalues = df[i, "age"] # For which x-values (age)
        , cent = c(50)
        , calibration = FALSE
      )

      cent50_ig <- centiles.pred(
        obj = mod_ig
        , type = "centiles"
        , xname = "Age"
        , xvalues = df[i, "age"] # For which x-values (age)
        , cent = c(50)
        , calibration = FALSE
      )

      df$cent50_avacc[i] <- round(cent50_avacc$`50`[1],2)
      df$cent50_ig[i] <- round(cent50_ig$`50`[1],3)

    }

    final_df <- df %>%

      # Calculate avacc and ig as percentage of predicted

      mutate(avacc_perc_pred = round(abs(cent50_avacc[i])/abs(avacc)*100, 2)) %>%
      mutate(ig_perc_pred = round(abs(cent50_ig[i])/abs(ig)*100,2)) %>%
      mutate(avacc_z = round(z.scores(mod_avacc, x= age, y= avacc),2)) %>%
      mutate(ig_z = round(z.scores(mod_ig, x = age, y= ig),2))

    #head(final_df[0:4,])

    #    final_df

  })

  output$modifiedData <- DT::renderDataTable({
    DT::datatable(calc_50_perc_uploaded(), options = list(dom = 't'))
  })


  #==========================================================================================
  # Set up data frame to hold results for individual inputs
  #==========================================================================================

  create_result_frame_r <- eventReactive(input$Calculate_r, {

    req(input$upload)

    df <- read_userdata()

    # Make sure that ages are between 20 and 89, inclusive.
    shiny::validate(
      need(all(na.omit(df$age) >= 20) && all(na.omit(df$age) <= 89), "Please enter ages between 20 and 89 (inclusive).")
    )

    # print(df)

    # Reshape from wide to long, sort according name

    res_frame <- df %>%
      pivot_longer(cols = c("avacc", "ig"), values_to = "values") %>%
      dplyr::arrange(name) %>%
      dplyr::rename(
        gender = sex
        , parameter = name
      ) %>%
      mutate(
        model_name = NA
        , percentile = NA
      )

    # print(res_frame)

    res_frame$model_name <- rep(paste0("centile", "_", res_frame$parameter, "_", res_frame$gender))

    res_frame

  })

  #------------------------------------------------------------------------------------------
  # Calculate the users' percentiles based on the input and model
  #------------------------------------------------------------------------------------------

  percentile_results_r <- eventReactive(input$Calculate_r, {

    req(input$upload)

    res_frame <- create_result_frame_r()

    for (i in seq_len(nrow(res_frame))) {

      current_model <- model_list[[res_frame$model_name[i]]]

      # Which centiles to plot
      # centiles_pred <- c(3, 15, 50, 85, 97)

      # Predict centiles and save them in a data frame
      z_score <- centiles.pred(
        obj = current_model
        , type = "z-scores"
        , xname = "Age"
        , xvalues = res_frame$age[i] # For which x-values (age)
        , yval = res_frame$values[i]
        , cent = c(3, 15, 50, 85, 97)
        , calibration = FALSE
      )

      res_frame$percentile[i] <- round(pnorm(z_score), 3)*100

    }

    as.data.frame(res_frame)


  })

  #------------------------------------------------------------------------------------------
  # Create the plots based on the users' input
  #------------------------------------------------------------------------------------------


  refplot_r <- eventReactive(input$Calculate_r, {
    withProgress(message = 'Making plot', value = 0, {
      create_plot_g(
        mod = model_list[grep(paste0("(centile_)(.*)(_)", "(m|f)", "($)"), names(model_list))]
        , input_frame = percentile_results_r()
      )

    })}

  )
















  # Prepare file for download

  output$download_perc_pred <- downloadHandler(

    filename = function() {paste0("avacc_ig_perc_pred.csv",  Sys.Date(), ".csv")},
    content = function(file){
      write.table(calc_50_perc_uploaded(), file, row.names = FALSE, sep = ",")
    }
  )



  #==========================================================================================
  # Set up data frame to hold results for individual inputs
  #==========================================================================================

  create_result_frame_i <- eventReactive(input$Calculate_i, {

    # Some checks
    # Do we have an age for each measurement?

    # Make sure that ages are between 20 and 89, inclusive.
    shiny::validate(
      need(all(na.omit(age_i()) >= 20 && na.omit(age_i()) <= 89), "Please enter ages between 20 and 89 (inclusive).")
    )

    param_list <- c("avacc", "ig")

    res_frame <- data.frame(
      parameter = param_list
      , age = rep(age_i(), times = length(param_list))
      , gender = rep(sex_i(), times = length(param_list))
      , values = c( # ATTENTION: Must have same order as variables in "param_list"!
        avacc_i()
        , ig_i()
      )
      , model_name = rep(paste0("centile", "_", param_list, "_", sex_i()))
      , percentile = NA
      , stringsAsFactors = FALSE
    )

  })

  #------------------------------------------------------------------------------------------
  # Calculate the users' percentiles based on the input and model
  #------------------------------------------------------------------------------------------

  percentile_results_i <- eventReactive(input$Calculate_i, {

    res_frame <- create_result_frame_i()

    # print(res_frame)

    for (i in seq_len(nrow(res_frame))) {

      current_model <- model_list[[res_frame$model_name[i]]]

      # Which centiles to plot
      # centiles_pred <- c(3, 15, 50, 85, 97)

      # Predict centiles and save them in a data frame
      z_score <- centiles.pred(
        obj = current_model
        , type = "z-scores"
        , xname = "Age"
        , xvalues = res_frame$age[i] # For which x-values (age)
        , yval = res_frame$values[i]
        , cent = c(3, 15, 50, 85, 97)
        , calibration = FALSE
      )

      res_frame$percentile[i] <- round(pnorm(z_score), 3)*100

    }

    res_frame

    # print(res_frame)

  })

  #------------------------------------------------------------------------------------------
  # Create the plots based on the users' input
  #------------------------------------------------------------------------------------------

  refplot_i <- eventReactive(input$Calculate_i, {
    withProgress(message = 'Making plot', value = 0, {
      create_plot_i(
        mod = model_list[grep(paste0("(centile_)(.*)(_)(", sex_i(), ")($)"), names(model_list))]
        , input_frame = percentile_results_i()
      )

    })})



  #==========================================================================================
  # Cohort-level data stratified
  #==========================================================================================
  #------------------------------------------------------------------------------------------
  # Set up data frame to hold results for stratified cohort-data
  #------------------------------------------------------------------------------------------

  create_result_frame_g <- eventReactive(input$Calculate_g, {

    # Some checks
    # Do we have an age for each measurement?

    # Make sure that ages are between 20 and 89, inclusive.
    shiny::validate(
      need(all(na.omit(c(age_m(), age_f())) >= 20) && all(na.omit(c(age_m(), age_f())) <= 89), "Please enter ages between 20 and 89 (inclusive).")
    )

    param_list <- c("avacc", "ig")

    res_frame <- data.frame(
      parameter = param_list
      , age = rep(c(age_m(), age_f()), each = length(param_list))
      , gender = rep(c("m", "f"), each = length(param_list))
      , values = c( # ATTENTION: Must have same order as variables in "param_list"!
        avacc_m()
        , ig_m()
        , avacc_f()
        , ig_f()
      )
      , model_name = paste0("centile", "_", c("avacc", "ig"), "_", rep(c("m", "f"), each = 2))
      , percentile = NA
      , stringsAsFactors = FALSE
    )

    res_frame

  })


  #------------------------------------------------------------------------------------------
  # Calculate the users' percentiles based on the input and model
  #------------------------------------------------------------------------------------------

  percentile_results_g <- eventReactive(input$Calculate_g, {

    res_frame <- create_result_frame_g()

    for (i in seq_len(nrow(res_frame))) {

      current_model <- model_list[[res_frame$model_name[i]]]

      # Which centiles to plot
      # centiles_pred <- c(3, 15, 50, 85, 97)

      # Predict centiles and save them in a data frame
      z_score <- centiles.pred(
        obj = current_model
        , type = "z-scores"
        , xname = "Age"
        , xvalues = res_frame$age[i] # For which x-values (age)
        , yval = res_frame$values[i]
        , cent = c(3, 15, 50, 85, 97)
        , calibration = FALSE
      )

      res_frame$percentile[i] <- round(pnorm(z_score), 3)*100

    }

    # print(res_frame)

    res_frame

  })

  #------------------------------------------------------------------------------------------
  # Create the plots based on the users' input
  #------------------------------------------------------------------------------------------

  refplot_g <- eventReactive(input$Calculate_g, {
    create_plot_g(
      mod = model_list[grep(paste0("(centile_)(.*)(_)", "(m|f)", "($)"), names(model_list))]
      , input_frame = percentile_results_g()
    )

  })

  #==========================================================================================
  # Cohort-level data not stratified
  #==========================================================================================
  #------------------------------------------------------------------------------------------
  # Set up data frame to hold results for non-stratified cohort-data
  #------------------------------------------------------------------------------------------

  create_result_frame_b <- eventReactive(input$Calculate_b, {

    # Some checks
    # Do we have an age for each measurement?

    # Make sure that ages are between 20 and 89, inclusive.
    shiny::validate(
      need(all(na.omit(age_b()) >= 20 && na.omit(age_b()) <= 89), "Please enter ages between 20 and 89 (inclusive).")
    )

    param_list <- c("avacc", "ig")

    res_frame <- data.frame(
      parameter = param_list
      , age = rep(age_b(), times = length(param_list))
      # , gender = rep("b", times = length(param_list))
      , values = c( # ATTENTION: Must have same order as variables in "param_list"!
        avacc_b()
        , ig_b()
      )
      , model_name = rep(paste0("centile", "_", param_list, "_b"))
      , percentile = NA
      , stringsAsFactors = FALSE
    )

    res_frame

  })

  #------------------------------------------------------------------------------------------
  # Calculate the users' percentiles based on the input and model
  #------------------------------------------------------------------------------------------

  percentile_results_b <- eventReactive(input$Calculate_b, {

    res_frame <- create_result_frame_b()

    for (i in seq_len(nrow(res_frame))) {

      current_model <- model_list[[res_frame$model_name[i]]]

      # Which centiles to plot
      # centiles_pred <- c(3, 15, 50, 85, 97)

      # Predict centiles and save them in a data frame
      z_score <- centiles.pred(
        obj = current_model
        , type = "z-scores"
        , xname = "Age"
        , xvalues = res_frame$age[i] # For which x-values (age)
        , yval = res_frame$values[i]
        # , cent = c(3, 15, 50, 85, 97)
        , calibration = FALSE
      )

      res_frame$percentile[i] <- round(pnorm(z_score), 3)*100

    }

    # print(res_frame)

    res_frame

  })

  #------------------------------------------------------------------------------------------
  # Create the plots based on the users' input
  #------------------------------------------------------------------------------------------

  refplot_b <- eventReactive(input$Calculate_b, {
    create_plot_i( # We use the same function as for the individual data to create the plots
      mod = model_list[grep(paste0("(centile_)(.*)(_)", "(b)", "($)"), names(model_list))]
      , input_frame = percentile_results_b()
    )

  })

  plotHolder <- reactiveValues(plot = NULL)

  observeEvent(input$Calculate_i, {
    plotHolder$plot <- refplot_i()
  })

  observeEvent(input$Calculate_g, {
    plotHolder$plot <- refplot_g()
  })

  observeEvent(input$Calculate_b, {
    plotHolder$plot <- refplot_b()
  })

  observeEvent(input$Calculate_r, {
    plotHolder$plot <- refplot_r()
  })

  output$plot1 <- renderPlot({
    plotHolder$plot
  }#, width = 1000, height = 1300
  )

  # output$plot_r <- renderPlot({
  #   plotHolder$plot_r
  # }#, width = 1000, height = 1300
  # )

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("plot1")
  })

  observeEvent(input$Calculate_i, {
    shinyjs::show("plot1")
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("plot1")
  })

  observeEvent(input$Calculate_b, {
    shinyjs::show("plot1")
  })

  observeEvent(input$Calculate_r, {
    shinyjs::show("plot1")
  })


  observeEvent(input$Calculate_i, {
  output$download_plot <- downloadHandler(
    filename = "Percentile_curves.png",
    content = function(file) {
      # save the plot as a PNG file
      png(file)
      print(plotHolder$plot)
      dev.off()
    }
  )})

  observeEvent(input$Calculate_g, {
    output$download_plot <- downloadHandler(
      filename = "Percentile_curves.png",
      content = function(file) {
        # save the plot as a PNG file
        png(file)
        print(plotHolder$plot)
        dev.off()
      }
    )})

    observeEvent(input$Calculate_b, {
      output$download_plot <- downloadHandler(
        filename = "Percentile_curves.png",
        content = function(file) {
          # save the plot as a PNG file
          png(file)
          print(plotHolder$plot)
          dev.off()
        }
      )})

    observeEvent(input$Calculate_r, {
      output$download_plot <- downloadHandler(
        filename = "Percentile_curves.png",
        content = function(file) {
          # save the plot as a PNG file
          png(file)
          print(plotHolder$plot)
          dev.off()
        }
      )})



  # #==========================================================================================
  # # Cohort-level data stratified (raw level)
  # #==========================================================================================
  # #------------------------------------------------------------------------------------------
  # # Set up data frame to hold results for stratified cohort-data
  # #------------------------------------------------------------------------------------------
  # # create button that triggers creating plots
  # create_result_frame_r <- eventReactive(input$Calculate_r, {
  #
  #   # Some checks
  #   # Do we have an age for each measurement?
  #
  #   # Make sure that ages are between 20 and 89, inclusive.
  #   shiny::validate(
  #     need(all(na.omit(age()) >= 20) && all(na.omit(age()) <= 89), "Please enter ages between 20 and 89 (inclusive).")
  #   )
  #
  #   param_list <- c("avacc", "ig")
  #
  #   res_frame <- data.frame(
  #     parameter = param_list
  #     , age = rep(age(), each = length(param_list))
  #     , gender = rep(c("m", "f"), each = length(param_list))
  #     , values = c( # ATTENTION: Must have same order as variables in "param_list"!
  #       avacc()
  #       , ig()
  #     )
  #     , model_name = paste0("centile", "_", c("avacc", "ig"), "_", rep(c("m", "f"), each = 2))
  #     , percentile = NA
  #     , stringsAsFactors = FALSE
  #   )
  #
  #   res_frame
  #
  # })
  #
  #
  # #------------------------------------------------------------------------------------------
  # # Calculate the users' percentiles based on the input and model
  # #------------------------------------------------------------------------------------------
  #
  # percentile_results_r <- eventReactive(input$Calculate_r, {
  #
  #   res_frame <- create_result_frame_r()
  #
  #   for (i in seq_len(nrow(res_frame))) {
  #
  #     current_model <- model_list[[res_frame$model_name[i]]]
  #
  #     # Which centiles to plot
  #     # centiles_pred <- c(3, 15, 50, 85, 97)
  #
  #     # Predict centiles and save them in a data frame
  #     z_score <- centiles.pred(
  #       obj = current_model
  #       , type = "z-scores"
  #       , xname = "Age"
  #       , xvalues = res_frame$age[i] # For which x-values (age)
  #       , yval = res_frame$values[i]
  #       , cent = c(3, 15, 50, 85, 97)
  #       , calibration = FALSE
  #     )
  #
  #     res_frame$percentile[i] <- round(pnorm(z_score), 3)*100
  #
  #   }
  #
  #   # print(res_frame)
  #
  #   res_frame
  #
  # })
  #
  # #------------------------------------------------------------------------------------------
  # # Create the plots based on the users' input
  # #------------------------------------------------------------------------------------------
  #
  # refplot_r <- eventReactive(input$Calculate_r, {
  #
  #   create_plot_g(
  #     mod = model_list[grep(paste0("_", "m|f", "$"), names(model_list))]
  #     , input_frame = percentile_results_r()
  #   )
  #
  # }
  # )


  #==========================================================================================
  # Calculate 50. percentiles based on input
  #==========================================================================================

  calc_50_perc <- eventReactive(input$Calculate_i, {

    param_list <- c("avacc", "ig")

    perc50_frame <- data.frame(
      parameter = param_list
      , age = age_i()
      , sex = sex_i()
      , perc50 = NA
    )

    for (i in seq_len(length(param_list))) {

      current_model <- model_list[[grep(paste0("centile_", param_list[i], "_", sex_i()), names(model_list))]]

      # Which centiles to plot
      # centiles_pred <- c(3, 15, 50, 85, 97)

      # Predict centiles and save them in a data frame
      cent50 <- centiles.pred(
        obj = current_model
        , type = "centiles"
        , xname = "Age"
        , xvalues = age_i() # For which x-values (age)
        , cent = c(50)
        , calibration = FALSE
      )

      perc50_frame$perc50[i] <- cent50[[2]]

    }

    perc50_frame

  })



  #==========================================================================================
  # Create collapsible boxes for "3) Translation of results" - combined recommendations
  #==========================================================================================

  observeEvent(input$Calculate_i | input$Calculate_g | input$Calculate_b, ({
    updateCollapse(session, "coll_translation", open = "Panel 1")
  }))



  #==========================================================================================
  # Translation of results - calculations # Set this inactive as the prediction equation for VO2max does not seem accurate enough for this to make sense
  #==========================================================================================


  # # Predict VO2max
  # datasetInput_0 <- eventReactive(input$Calculate_i, {
  #
  #   # Create a data frame that holds all relevant user input values for the prediction model
  #   prediction_frame <- data.frame(
  #     Age = age_i()
  #     , Sex = ifelse(sex_i() %in% "f", 1, 0)
  #     , BMI = weight_i()/(height_i()*100)^2
  #     , ig_gradient_pla = ig_i()
  #     , ACC_day_mg_pla = avacc_i()
  #     , stringsAsFactors = FALSE
  #   )
  #
  #   # print(prediction_frame)
  #
  #   # Use the model named "mod" in the model list to predict VO2peak
  #   vo2pred <- predict(model_list$mod, newdata = prediction_frame)
  #
  #   # p1 <- predict(model_list$mod, newdata = data.frame(
  #   #   Age = 50
  #   #   , Sex = 1
  #   #   , BMI = 70/(1.75^2)
  #   #   , ig_gradient_pla = -2.6
  #   #   , ACC_day_mg_pla = 40
  #   # ))
  #   #
  #   # p2 <- predict(model_list$mod, newdata = data.frame(
  #   #   Age = 50
  #   #   , Sex = 1
  #   #   , BMI = 70/(1.75^2)
  #   #   , ig_gradient_pla = -2.6
  #   #   , ACC_day_mg_pla = 42.75352
  #   # ))
  #   #
  #   # p1*1.01
  #
  #   vo2pred
  #
  #   # cat("\nPredicted VO2peak:", vo2pred, "\n") # For debugging
  #
  # })
  #
  # output$output_0 <- renderText({
  #   datasetInput_0()
  # })


  # Calculate how much ACC or IG have to change in order to achieve a specified percentage increase in VO2max

  datasetIncrease <- eventReactive(input$Calculate_i, {

    # Create a data frame that holds all relevant user input values for the prediction model
    prediction_frame <- data.frame(
      Age = age_i()
      , Sex = ifelse(sex_i() %in% "f", 1, 0)
      , BMI = weight_i()/(height_i()*100)^2
      , ig_gradient_pla = ig_i()
      , ACC_day_mg_pla = avacc_i()
      , stringsAsFactors = FALSE
    )

    # print(prediction_frame)

    # What ACC is necessary to increase VO2max by 3.5%, holding everything else fixed?
    inc_acc <- find_delta_x(
      mod = model_list$mod
      , myvals = prediction_frame
      , fix = c("Age", "Sex", "BMI", "ig_gradient_pla")
      , delta_y_perc = 0.035
    )

    # What IG is necessary to increase VO2max by 3.5%, holding everything else fixed?
    inc_ig <- find_delta_x(
      mod = model_list$mod
      , myvals = prediction_frame
      , fix = c("Age", "Sex", "BMI", "ACC_day_mg_pla")
      , delta_y_perc = 0.035
    )

    #cat("\nDelta x for 3.5% VO2max-change: ", c(inc_acc - avacc_i(), inc_ig - ig_i()), "\n")

    c(inc_acc - avacc_i(), inc_ig - ig_i())

  })

  output$inc_perc <- eventReactive(input$Calculate_i, {
    datasetIncrease()
  })


  ##############################################################################
  # Calculate how much avacc is necessary to increase VO2max by 1 ml
  ##############################################################################

  # Calculate how much ACC or IG have to change in order to achieve a specified percentage increase in VO2max

  # For individual data


  #############################
  datasetIncrease_0_i <- eventReactive(input$Calculate_i, {

    # Create a data frame that holds all relevant user input values for the prediction model
    prediction_frame <- data.frame(
      # Age = age_i()
      # , Sex = ifelse(sex_i() %in% "f", 1, 0)
      # , BMI = weight_i()/(height_i()/100)^2
      AD_ig_gradient_ENMO_0.24hr = ig_i()
      , AD_mean_ENMO_mg_0.24hr = avacc_i()
      , stringsAsFactors = FALSE
    )

    # print(prediction_frame)

    # Females
    # What ACC is necessary to increase VO2max by 1 ml, holding everything else fixed?
     if (sex_i() == "f") {

     inc_acc_abs_f <- find_delta_y(
      mod = model_list$model_VO2_avacc_f
      , myvals = prediction_frame[1,"AD_mean_ENMO_mg_0.24hr", drop = FALSE]
      , fix = ""
      , delta_y_abs = 1
    )

     }
     else if (sex_i() == "m") {


    # Males
    # What ACC is necessary to increase VO2max by 1 ml, holding everything else fixed?
    inc_acc_abs_m <- find_delta_y(
      mod = model_list$model_VO2_avacc_m
      , myvals = prediction_frame[1,"AD_mean_ENMO_mg_0.24hr", drop = FALSE]
      , fix = ""
      , delta_y_abs = 1
    )
     }

    # Females
    # What IG is necessary to increase VO2max by 1 ml, holding everything else fixed?
     if (sex_i() == "f") {

     inc_ig_abs_f <- find_delta_y_ig(
      mod = model_list$model_VO2_ig_f
      , myvals = prediction_frame[1,"AD_ig_gradient_ENMO_0.24hr", drop = FALSE]
      , fix = ""
      , delta_y_abs = 1
    )

     }
     else if (sex_i() == "m") {
    # Males
    # What IG is necessary to increase VO2max by 1 ml, holding everything else fixed?
    inc_ig_abs_m <- find_delta_y_ig(
      mod = model_list$model_VO2_ig_m
      , myvals = prediction_frame[1,"AD_ig_gradient_ENMO_0.24hr", drop = FALSE]
      , fix = ""
      , delta_y_abs = 1
    )
     }

    # if (sex_i() == "f") {
    #   cat("\nDelta x for 1 mL VO2max-change _ f:", c(inc_acc_abs_f - avacc_i(), inc_ig_abs_f - ig_i()), "\n")
    # }
    # else if (sex_i() == "m") {
    #   cat("\nDelta x for 1 mL VO2max-change _ m: ", c(inc_acc_abs_m - avacc_i(), inc_ig_abs_m - ig_i()), "\n")
    # }

     # cat("\nDelta x for 1 mL VO2max-change: ", c(inc_acc_abs_f - avacc_i(), inc_ig_abs_f - ig_i()), "\n")
     # cat("\nDelta x for 1 mL VO2max-change: ", c(inc_acc_abs_m - avacc_i(), inc_ig_abs_m - ig_i()), "\n")


    if (sex_i() == "f") {
      paste0("Average acceleration: ", round(inc_acc_abs_f - avacc_i(), digits = 1), ", or intensity gradient: ", round(inc_ig_abs_f - ig_i(), digits = 3))
    }
    else if (sex_i() == "m") {
      paste0("Average acceleration: ", round(inc_acc_abs_m - avacc_i(), digits = 1), ", or intensity gradient: ", round(inc_ig_abs_m - ig_i(), digits = 3))
    }



    #cat("\nDelta x for 1 mL VO2max-change: ", c(inc_acc_abs - avacc_i(), inc_ig_abs - ig_i()), "\n")

    # paste0("Average acceleration: ", round(inc_acc_abs - avacc_i(), digits = 1), ", or Intensity gradient: ", round(inc_ig_abs - ig_i(), digits = 2))

  })

  # For stratified cohort

  datasetIncrease_0_g <- eventReactive(input$Calculate_g, {

    prediction_frame <- data.frame(
      # Age = c(age_m(), age_f())
      # , Sex = c(0)
      # , BMI = c(weight_m()/(height_m()/100)^2, weight_f()/(height_f()/100)^2)
      AD_ig_gradient_ENMO_0.24hr = c(ig_m(), ig_f())
      , AD_mean_ENMO_mg_0.24hr = c(avacc_m(), avacc_f())
      , stringsAsFactors = FALSE
    )

  #print(prediction_frame)
    # cat("Where here!\n")

    # for(i in seq_len(nrow(prediction_frame))) {

    # What ACC is necessary to increase VO2max by 1 ml, holding everything else fixed?
    inc_acc_abs_m <- find_delta_y(
      mod = model_list$model_VO2_avacc_m
      , myvals = prediction_frame[1, "AD_mean_ENMO_mg_0.24hr", drop = FALSE]
      , fix = ""
      , delta_y_abs = 1
    )

    inc_acc_abs_f <- find_delta_y(
      mod = model_list$model_VO2_avacc_f
      , myvals = prediction_frame[2, "AD_mean_ENMO_mg_0.24hr", drop = FALSE]
      , fix = ""
      , delta_y_abs = 1
    )

    # What IG is necessary to increase VO2max by 1 ml, holding everything else fixed?
    inc_ig_abs_m <- find_delta_y_ig(
      mod = model_list$model_VO2_ig_m
      , myvals = prediction_frame[1, "AD_ig_gradient_ENMO_0.24hr", drop = FALSE]
      , fix = ""
      # , fix = c("Age", "Sex", "BMI", "ACC_day_mg_pla")
      , delta_y_abs = 1
    )

    inc_ig_abs_f <- find_delta_y_ig(
      mod = model_list$model_VO2_ig_f
      , myvals = prediction_frame[2, "AD_ig_gradient_ENMO_0.24hr", drop = FALSE]
      # , fix = c("Age", "Sex", "BMI", "ACC_day_mg_pla")
      , fix = ""
      , delta_y_abs = 1
    )
    # }

    # print(inc_acc_abs_m)
    # print(inc_acc_abs_f)
    # print(inc_ig_abs_m)
    # print(inc_ig_abs_f)

    paste0("Males: average acceleration ", round(inc_acc_abs_m - avacc_m(), digits = 2), ", or intensity gradient: ", round(inc_ig_abs_m - ig_m(), digits = 3), "\nFemales: average acceleration ", round(inc_acc_abs_f - avacc_f(), digits = 2), ", or intensity gradient: ", round(inc_ig_abs_f - ig_f(), digits = 3))

  })

  # For non-stratified cohort

  datasetIncrease_0_b <- eventReactive(input$Calculate_b, {

    prediction_frame <- data.frame(
      Age = c(age_b())
      , Sex = c(0.5)
      , BMI = c(weight_b()/(height_b()/100)^2)
      , ig_gradient_pla = c(ig_b())
      , ACC_day_mg_pla = c(avacc_b())
      , stringsAsFactors = FALSE
    )

    #print(prediction_frame)

    # for(i in seq_len(nrow(prediction_frame))) {

    # What ACC is necessary to increase VO2max by 1 ml, holding everything else fixed?
    inc_acc_abs <- find_delta_y(
      mod = model_list$mod
      , myvals = prediction_frame[1, ]
      , fix = c("Age", "Sex", "BMI", "ig_gradient_pla")
      , delta_y_abs = 1
    )

    # What IG is necessary to increase VO2max by 1 ml, holding everything else fixed?
    inc_ig_abs <- find_delta_y_ig(
      mod = model_list$mod
      , myvals = prediction_frame[1, ]
      , fix = c("Age", "Sex", "BMI", "ACC_day_mg_pla")
      , delta_y_abs = 1
    )
    # }

    paste0("Average acceleration: ", round(inc_acc_abs - avacc_b(), digits = 1), ", or Intensity gradient: ", round(inc_ig_abs - ig_b(), digits = 2))

  })

  inc_abs_holder <- reactiveValues(frame = NULL) # Holds result-frame

  observeEvent(input$Calculate_i, {
    inc_abs_holder$frame <- datasetIncrease_0_i()
  })

  observeEvent(input$Calculate_g, {
    inc_abs_holder$frame <- datasetIncrease_0_g()
  })

  observeEvent(input$Calculate_b, {
    inc_abs_holder$frame <- "This function is not available for non-stratified data." #datasetIncrease_0_b()
  })

  output$inc_abs <- reactive({
    inc_abs_holder$frame
  })










































  ###############################################################################

  # How much activity is needed to increase avacc by 1mg

  # Individual data

  datasetInput_cvd <- reactive({

    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - input$avacc_i,
      walk_slw = round((1440*diff_avacc_abs)/min_avacc_80), digits=0)
  })

  output$output1_cvd <- renderText({

      isolate(paste0("Slow walking: ", datasetInput_cvd()$walk_slw, " min"))
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output1_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output1_cvd")
  })

  # Brisk walking
  datasetInput_1_cvd <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_i,
      walk_brsk = round((1440*diff_avacc_abs)/min_avacc_175),digits=0)
  })



  output$output2_cvd <- renderText({
      isolate(paste0("Brisk walking: ", datasetInput_1_cvd()$walk_brsk, " min"))
    })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output2_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output2_cvd")
  })




  # Fast walking
  datasetInput_2_cvd <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_i,
      walk_fst = round((1440*diff_avacc_abs)/min_avacc_400), digits=0)


  })


  output$output3_cvd <- renderText({

      isolate(paste0("Fast walking: ", datasetInput_2_cvd()$walk_fst, " min"))
    })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output3_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output3_cvd")
  })

  # Slow running

  datasetInput_3_cvd <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750-input$avacc_i,
      run_slw = round((1440*diff_avacc_abs)/min_avacc_750), digits=0)
  })



  output$output4_cvd <- renderText({
      isolate(paste0("Slow running: ", datasetInput_3_cvd()$run_slw, " min"))
    })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output4_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output4_cvd")
  })



  # Moderate running

  datasetInput_4_cvd <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000 - input$avacc_i,
      run_mod = round((1440*diff_avacc_abs)/min_avacc_1000), digits=0)

  })

  output$output5_cvd <- renderText({
    isolate(paste0("Moderate running: ", datasetInput_4_cvd()$run_mod, " min"))
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output5_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output5_cvd")
  })







#===============================================================================
  # How much activity is needed to increase avacc by 1mg

  # Group-level data - stratified
#===============================================================================


  # Slow walking

  datasetInput_g_0_m_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - avacc_m(),
      walk_slw = round((1440*diff_avacc_abs)/min_avacc_80), digits=0)
  })

  output$output1_m_cvd <- renderText({isolate(paste0("Males, slow walking: ",
                                                 datasetInput_g_0_m_cvd()$walk_slw, " min"))})


  datasetInput_g_0_f_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - avacc_f(),
      walk_slw = round((1440*diff_avacc_abs)/min_avacc_80), digits=0)
  })

  output$output1_f_cvd <- renderText({isolate(paste0("Females, slow walking: ",
                                                 datasetInput_g_0_f_cvd()$walk_slw, " min"))})

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output1_m_cvd")
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("output1_f_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output1_m_cvd")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output1_f_cvd")
  })

  # Brisk walking

  datasetInput_g_1_m_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - avacc_m(),
      walk_brsk = round((1440*diff_avacc_abs)/min_avacc_175),digits=0)
  })

  output$output2_m_cvd <- renderText({isolate(paste0("Males, brisk walking: ",
                                                 datasetInput_g_1_m()$walk_brsk, " min"))})


  datasetInput_g_1_f_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - avacc_f(),
      walk_brsk = round((1440*diff_avacc_abs)/min_avacc_175),digits=0)
  })

  output$output2_f_cvd <- renderText({isolate(paste0("Females, brisk walking: ",
                                                 datasetInput_g_1_f_cvd()$walk_brsk, " min"))})

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output2_m_cvd")
  })
  observeEvent(input$Calculate_g, {
    shinyjs::show("output2_f_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output2_m_Cvd")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output2_f_cvd")
  })

  # Fast walking

  datasetInput_g_2_m_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - avacc_m(),
      walk_fst = round((1440*diff_avacc_abs)/min_avacc_400), digits=0)
  })

  output$output3_m_cvd <- renderText({isolate(paste0("Males, fast walking: ",
                                                 datasetInput_g_2_m_cvd()$walk_fst, " min"))})



  datasetInput_g_2_f_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - avacc_f(),
      walk_fst = round((1440*diff_avacc_abs)/min_avacc_400), digits=0)
  })

  output$output3_f_cvd <- renderText({isolate(paste0("Females, fast walking: ",
                                                     datasetInput_g_2_f_cvd()$walk_fst, " min"))})

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output3_m_cvd")
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("output3_f_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output3_m_cvd")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output3_f_cvd")
  })


  # Slow running

  datasetInput_g_3_m_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750-avacc_m(),
      run_slw = round((1440*diff_avacc_abs)/min_avacc_750),digits=0)
  })

  output$output4_m_cvd <- renderText({isolate(paste0("Males, slow running: ",
                                                 datasetInput_g_3_m_cvd()$run_slw, " min"))})


  datasetInput_g_3_f_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750-avacc_f(),
      run_slw = round((1440*diff_avacc_abs)/min_avacc_750),digits=0)
  })

  output$output4_f_cvd <- renderText({isolate(paste0("Females, slow running: ",
                                                 datasetInput_g_3_f_cvd()$run_slw, " min"))})

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output4_m_cvd")
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("output4_f_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output4_m_cvd")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output4_f_cvd")
  })


  # Moderate running


  datasetInput_g_4_m_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000-avacc_m(),
      run_mod = round((1440*diff_avacc_abs)/min_avacc_1000), digits=0)
  })

  output$output5_m_cvd <- renderText({isolate(paste0("Males, moderate running: ",
                                                 datasetInput_g_4_m_cvd()$run_mod, " min"))})


  datasetInput_g_4_f_cvd <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000-avacc_f(),
      run_mod = round((1440*diff_avacc_abs)/min_avacc_1000), digits=0)
  })

  output$output5_f_cvd <- renderText({isolate(paste0("Females, moderate running: ",
                                                 datasetInput_g_4_f_cvd()$run_mod, " min"))})

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output5_m_cvd")
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("output5_f_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output5_m_cvd")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output5_f_cvd")
  })




  #===============================================================================
  # How much activity is needed to increase avacc by 1mg

  # Group-level data - non-stratified
  #===============================================================================

  # Slow walking

  datasetInput_b_cvd <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - input$avacc_b,
      walk_slw = round((1440*diff_avacc_abs)/min_avacc_80), digits=0)
  })


  output$output1_b_cvd <- renderText({
      isolate(paste0("Slow walking: ", datasetInput_b_cvd()$walk_slw, " min"))
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output1_b_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output1_b_cvd")
  })

  # # Brisk walking

  datasetInput_1_b_cvd <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_b,
      walk_brsk = round((1440*diff_avacc_abs)/min_avacc_175),digits=0)
  })

  output$output2_b_cvd <- renderText({
      isolate(paste0("Brisk walking: ", datasetInput_1_b_cvd()$walk_brsk, " min"))
    })


  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output2_b_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output2_b_cvd")
  })



  # Fast walking

  datasetInput_2_b_cvd <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_b,
      walk_fst = round((1440*diff_avacc_abs)/min_avacc_400), digits=0)
  })

  output$output3_b_cvd <- renderText({
      isolate(paste0("Fast walking: ", datasetInput_2_b_cvd()$walk_fst, " min"))

    })


  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output3_b_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output3_b_cvd")
  })

  # Slow running

  datasetInput_3_b_cvd <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750-input$avacc_b,
      run_slw = round((1440*diff_avacc_abs)/min_avacc_750), digits=0)
  })

  output$output4_b_cvd <- renderText({
      isolate(paste0("Slow running: ", datasetInput_3_b_cvd()$run_slw, " min"))
    })


  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output4_b_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output4_b_cvd")
  })


  # Moderate running

  datasetInput_4_b_cvd <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = 1,
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000-input$avacc_b,
      run_mod = round((1440*diff_avacc_abs)/min_avacc_1000), digits=0)
  })

  output$output5_b_cvd <- renderText({
      isolate(paste0("Moderate running:", datasetInput_4_b_cvd()$run_mod, " min"))
   })


  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output5_b_cvd")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output5_b_cvd")
  })



















































































  ##############################################################################
  # Calculate how much avacc is necessary to increase VO2max by 3.5 ml/kg/min
  ##############################################################################

  # Calculate how much ACC or IG have to change in order to achieve a specified percentage increase in VO2max

  datasetIncrease_0_i_cvd <- eventReactive(input$Calculate_i, {

    # Create a data frame that holds all relevant user input values for the prediction model
    prediction_frame <- data.frame(
      Age = age_i()
      , Sex = ifelse(sex_i() %in% "f", 1, 0)
      , BMI = weight_i()/(height_i()/100)^2
      , ig_gradient_pla = ig_i()
      , ACC_day_mg_pla = avacc_i()
      , stringsAsFactors = FALSE
    )

    # print(prediction_frame)

    # What ACC is necessary to increase VO2max by 3.5 ml, holding everything else fixed?
    inc_acc_abs <- find_delta_cvd(
      mod = model_list$mod
      , myvals = prediction_frame
      , fix = c("Age", "Sex", "BMI", "ig_gradient_pla")
      , delta_y_abs = 3.5
    )

    # What IG is necessary to increase VO2max by 3.5 ml, holding everything else fixed?
    inc_ig_abs <- find_delta_cvd(
      mod = model_list$mod
      , myvals = prediction_frame
      , fix = c("Age", "Sex", "BMI", "ACC_day_mg_pla")
      , delta_y_abs = 3.5
    )

    #cat("\nDelta x for 3.5 mL VO2max-change: ", c(inc_acc_abs - avacc_i(), inc_ig_abs - ig_i()), "\n")

    paste0("Average acceleration: ", round(inc_acc_abs - avacc_i(), digits = 1), ", Intensity gradient: ", round(inc_ig_abs - ig_i(), digits = 2))

  })

  # For stratified cohort

  datasetIncrease_0_g_cvd <- eventReactive(input$Calculate_g, {

    prediction_frame <- data.frame(
      Age = c(age_m(), age_f())
      , Sex = c(0, 1)
      , BMI = c(weight_m()/(height_m()/100)^2, weight_f()/(height_f()/100)^2)
      , ig_gradient_pla = c(ig_m(), ig_f())
      , ACC_day_mg_pla = c(avacc_m(), avacc_f())
      , stringsAsFactors = FALSE
    )

  #  print(prediction_frame)

    # for(i in seq_len(nrow(prediction_frame))) {

    # What ACC is necessary to increase VO2max by 3.5 ml, holding everything else fixed?
    inc_acc_abs <- find_delta_cvd(
      mod = model_list$mod
      , myvals = prediction_frame[1, ]
      , fix = c("Age", "Sex", "BMI", "ig_gradient_pla")
      , delta_y_abs = 3.5
    )

    # What IG is necessary to increase VO2max by 3.5 ml, holding everything else fixed?
    inc_ig_abs <- find_delta_cvd(
      mod = model_list$mod
      , myvals = prediction_frame[1, ]
      , fix = c("Age", "Sex", "BMI", "ACC_day_mg_pla")
      , delta_y_abs = 3.5
    )
    # }


    paste0("Males: Average acceleration: ", round(inc_acc_abs - avacc_m(), digits = 1), ", Intensity gradient: ", round(inc_ig_abs - ig_m(), digits = 2), " Females: Average acceleration: ", round(inc_acc_abs - avacc_f(), digits = 1), ", Intensity gradient: ", round(inc_ig_abs - ig_f(), digits = 2))


  })


  # For non-stratified cohort

  datasetIncrease_0_b_cvd <- eventReactive(input$Calculate_b, {

    prediction_frame <- data.frame(
      Age = c(age_b())
      , Sex = c(0.5)
      , BMI = c(weight_b()/(height_b()/100)^2)
      , ig_gradient_pla = c(ig_b())
      , ACC_day_mg_pla = c(avacc_b())
      , stringsAsFactors = FALSE
    )

  #  print(prediction_frame)

    # for(i in seq_len(nrow(prediction_frame))) {

    # What ACC is necessary to increase VO2max by 3.5 ml, holding everything else fixed?
    inc_acc_abs <- find_delta_cvd(
      mod = model_list$mod
      , myvals = prediction_frame[1, ]
      , fix = c("Age", "Sex", "BMI", "ig_gradient_pla")
      , delta_y_abs = 3.5
    )



    # What IG is necessary to increase VO2max by 3.5 ml, holding everything else fixed?
    inc_ig_abs <- find_delta_cvd(
      mod = model_list$mod
      , myvals = prediction_frame[1, ]
      , fix = c("Age", "Sex", "BMI", "ACC_day_mg_pla")
      , delta_y_abs = 3.5    )
    # }


    paste0("Average acceleration: ", round(inc_acc_abs - avacc_b(), digits = 1), ", Intensity gradient: ", round(inc_ig_abs - ig_b(), digits = 2))

  })



  inc_abs_holder_cvd <- reactiveValues(frame = NULL) # Holds result-frame

  observeEvent(input$Calculate_i, {
    inc_abs_holder_cvd$frame <- datasetIncrease_0_i_cvd()
  })

  observeEvent(input$Calculate_g, {
    inc_abs_holder_cvd$frame <- datasetIncrease_0_g_cvd()
  })

  observeEvent(input$Calculate_b, {
    inc_abs_holder_cvd$frame <- datasetIncrease_0_b_cvd()
  })

  output$inc_abs_cvd <- reactive({
    inc_abs_holder_cvd$frame
  })




  ##############################################################################

  perc_50_i <- reactive({
    res_frame <- create_result_frame_i()

    # print(res_frame)

    for (i in seq_len(nrow(res_frame))) {

      current_model <- model_list[[res_frame$model_name[i]]]

      # Predict centiles and save them in a data frame
      z_score <- centiles.pred(
        obj = current_model
        , type = "z-scores"
        , xname = "Age"
        , xvalues = res_frame$age[i] # For which x-values (age)
        , yval = res_frame$values[i]
        , cent = c(50)
        , calibration = FALSE
      )

      res_frame$percentile[i] <- round(pnorm(z_score), 3)*100

    }

    res_frame

    # print(res_frame)
    #Create output including the percentile the individual is currently on
  })

  output$perc_50_avacc <- renderText({
    if(input$Calculate_i>0) {
      isolate(paste("Exact percentile of average acceleration: ",perc_50_i()$percentile[1]))
    }
  })

  output$perc_50_ig <- renderText({
    if(input$Calculate_i>0) {
      isolate(paste("Exact percentile of intensity gradient: ", perc_50_i()$percentile[2]))
    }
  })

  observeEvent(input$Calculate_i, {
    shinyjs::show("perc_50_avacc")
  })

  # observeEvent(input$Calculate_g, {
  #   shinyjs::show("perc_50_avacc_g")
  # })

  observeEvent(input$Calculate_b, {
    shinyjs::show("perc_50_avacc_b")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("perc_50_avacc")
  })

  # observeEvent(input$reset, {
  #   shinyjs::hide("perc_50_avacc_g")
  # })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("perc_50_avacc_b")
  })

  observeEvent(input$Calculate_i, {
    shinyjs::show("perc_50_ig")
  })

  # observeEvent(input$Calculate_g, {
  #   shinyjs::show("perc_50_ig_g")
  # })

  observeEvent(input$Calculate_b, {
    shinyjs::show("perc_50_ig_b")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("perc_50_ig")
  })

  # observeEvent(input$reset, {
  #   shinyjs::hide("perc_50_ig_g")
  # })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("perc_50_ig_b")
  })

  # For non-stratified data

  perc_50_b <- reactive({
    res_frame <- create_result_frame_b()

    # print(res_frame)

    for (i in seq_len(nrow(res_frame))) {

      current_model <- model_list[[res_frame$model_name[i]]]

      # Predict centiles and save them in a data frame
      z_score <- centiles.pred(
        obj = current_model
        , type = "z-scores"
        , xname = "Age"
        , xvalues = res_frame$age[i] # For which x-values (age)
        , yval = res_frame$values[i]
        # , cent = c(50)
        , calibration = FALSE
      )

      res_frame$percentile[i] <- round(pnorm(z_score), 3)*100

    }

    res_frame

  })

  ################################################################################
  #Create output including the percentile the individual is currently on - non-stratified data
  ################################################################################

  output$perc_50_avacc_b <- renderText({
    if(input$Calculate_b > 0) {
      isolate(paste("Exact percentile of average acceleration: ",perc_50_b()$percentile[1]))
    }
  })

  output$perc_50_ig_b <- renderText({
    if(input$Calculate_b > 0) {
      isolate(paste("Exact percentile of intensity gradient: ",perc_50_b()$percentile[2]))
    }
  })


  ################################################################################
  #Create output including the percentile the individual is currently on - stratified data
  ################################################################################

  perc_50_g <- reactive({

    res_frame <- create_result_frame_g()

    for (i in seq_len(nrow(res_frame))) {

      current_model <- model_list[[res_frame$model_name[i]]]

      # Predict centiles and save them in a data frame
      z_score <- centiles.pred(
        obj = current_model
        , type = "z-scores"
        , xname = "Age"
        , xvalues = res_frame$age[i] # For which x-values (age)
        , yval = res_frame$values[i]
        # , cent = c(50)
        , calibration = FALSE
      )

      res_frame$percentile[i] <- round(pnorm(z_score), 3)*100

    }

  #  print(res_frame)

    res_frame

    #Create output including the percentile the individual is currently on
  })

  output$perc_50_avacc_g <- renderText({
    res_frame <- perc_50_g()
    res_frame <- res_frame[res_frame$parameter %in% "avacc", ]

    if(input$Calculate_g > 0) {
      isolate(paste0("Exact percentile of average acceleration: ", "\nMales: ", res_frame$percentile[res_frame$gender %in% "m"], "\nFemales: ", res_frame$percentile[res_frame$gender %in% "f"]))
    }
  })

  output$perc_50_ig_g <- renderText({
    res_frame <- perc_50_g()
    res_frame <- res_frame[res_frame$parameter %in% "ig", ]

    if(input$Calculate_g > 0) {
      if(input$Calculate_g > 0) {
        isolate(paste0("Exact percentile of intensity gradient: ", "\nMales: ", res_frame$percentile[res_frame$gender %in% "m"], "\nFemales: ", res_frame$percentile[res_frame$gender %in% "f"]))
      }
    }
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("perc_50_avacc_g")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("perc_50_avacc_g")
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("perc_50_ig_g")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("perc_50_ig_g")
  })




  ###############################################################################
  # Translation of results for individual-level data
  ###############################################################################

  # mets for respective activities from light to vigorous (2.5, 3.4, 4, 7.5, 10.5),

  # For individual-level data

  # Slow walking

  datasetInput <- reactive({

    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - input$avacc_i,
      walk_slw = ifelse(perc_50_i()$percentile[1] < 50,
                        round((1440*diff_avacc_abs)/min_avacc_80),
                        round((1440*(input$avacc_i * 0.05))/min_avacc_80)),
      digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
    output$output1 <- renderText({
      isolate(paste0(datasetInput()$walk_slw, " min"))
    })
  })


  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output1")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output1")
  })

  # Brisk walking
  datasetInput_1 <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_i,
      walk_brsk = ifelse(perc_50_i()$percentile[1] < 50,
                         round((1440*diff_avacc_abs)/min_avacc_175),
                         round((1440*(input$avacc_i * 0.05))/min_avacc_175)),
      digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output2 <- renderText({
      isolate(paste0(datasetInput_1()$walk_brsk, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output2")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output2")
  })




  # Fast walking
  datasetInput_2 <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_i,
      walk_fst = ifelse(perc_50_i()$percentile[1] < 50,
                        round((1440*diff_avacc_abs)/min_avacc_400),
                        round((1440*(input$avacc_i * 0.05))/min_avacc_400)),
      digits=0)
  })




  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output3 <- renderText({
      isolate(paste0(datasetInput_2()$walk_fst, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output3")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output3")
  })

  # Slow running

  datasetInput_3 <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750-input$avacc_i,
      run_slw = ifelse(perc_50_i()$percentile[1] < 50,
                       round((1440*diff_avacc_abs)/min_avacc_750),
                       round((1440*(input$avacc_i * 0.05))/min_avacc_750)),
      digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output4 <- renderText({
      isolate(paste0(datasetInput_3()$run_slw, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output4")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output4")
  })



  # Moderate running

  datasetInput_4 <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000 - input$avacc_i,
      run_mod = ifelse(perc_50_i()$percentile[1] < 50,
                       round((1440*diff_avacc_abs)/min_avacc_1000),
                       round((1440*(input$avacc_i * 0.05))/min_avacc_1000)),
      digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output5 <- renderText({
    isolate(paste0(datasetInput_4()$run_mod, " min"))
  })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output5")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output5")
  })


  # Customisable acceleration
  datasetInput_custom <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_cust = custom_acc() - input$avacc_i,
      custom = ifelse(perc_50_i()$percentile[1] < 50,
                      round((1440*diff_avacc_abs)/min_avacc_cust),
                      round((1440*(input$avacc_i * 0.05))/min_avacc_cust)),
      digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output_cust <- renderText({
     paste0(datasetInput_custom()$custom, " min")
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output_cust")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output_cust")
  })


  ###############################################################################
  # Combined recommendation
  ###############################################################################

  # Code for checkboxes
  my_max <- 2
  my_min <- 2
  output$Selected <- renderText({
    paste(input$activities, collapse=",")
  })
  observe({
    if(length(input$activities) > my_max){
      updateCheckboxGroupInput(session, "activities", selected = tail(input$activities, my_max))
    }
    if(length(input$activities) < my_min){
      updateCheckboxGroupInput(session, "activities")
    }
  })



  # Slow walking - combined
  datasetInput_walk_slw <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80- input$avacc_i,
      walk_slw = ifelse(perc_50_i()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_80),
                        round(0.5*(1440*(input$avacc_i * 0.05))/min_avacc_80)),
      digits=0)
  })



  # Brisk walking
  datasetInput_walk_brsk <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_i,
      walk_brsk = ifelse(perc_50_i()$percentile[1] < 50,
           round(0.5*(1440*diff_avacc_abs)/min_avacc_175),
           round(0.5*(1440*(input$avacc_i * 0.05))/min_avacc_175)),
    digits=0)
  })

  # Fast walking
  datasetInput_walk_fst <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_i,
      walk_fst = ifelse(perc_50_i()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_400),
                        round(0.5*(1440*(input$avacc_i * 0.05))/min_avacc_400)),
      digits=0)
  })

  # Slow running
  datasetInput_run_slw <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750 - input$avacc_i,
      run_slw = ifelse(perc_50_i()$percentile[1] < 50,
                       round(0.5*(1440*diff_avacc_abs)/min_avacc_750),
                       round(0.5*(1440*(input$avacc_i * 0.05))/min_avacc_750)),
      digits=0)
  })

  # Moderate running
  datasetInput_run_mod <- reactive({
    perc_50_res <- calc_50_perc() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_i - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000 - input$avacc_i,
      run_mod = ifelse(perc_50_i()$percentile[1] < 50,
                       round(0.5*(1440*diff_avacc_abs)/min_avacc_1000),
                       round(0.5*(1440*(input$avacc_i * 0.05))/min_avacc_1000)),
      digits=0)
  })

  checkboxGroupInput("activities", "Variables:", c("Slow walking (3 kph)" = "1", "Brisk walking (5 kph)"="2", "Fast walking (6.5 kph)"="3", "Slow running (8 kph)"="4", "Moderate running (10 kph)"="5"))


  # If "Slow walking" is not selected: Show nothing
  observe({
   # input$intensity # set inactive since the intensity slider is currently not used
    output$output10 <- renderText({
      if ("1" %in% input$activities) {isolate(paste0("Slow walking: ",
                                                     datasetInput_walk_slw()$walk_slw, " min"))
      } else{
        ""
      }
    }
    )
  })

  # If "Brisk walking" is not selected: Show nothing
  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
    output$output11 <- renderText({
      if ("2" %in% input$activities) {isolate(paste0("Brisk walking: ",
                                                     datasetInput_walk_brsk()$walk_brsk, " min"))
      } else{
        ""
      }
    }
    )
  })

  # If "Fast walking" is not selected: Show nothing

  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
    output$output12 <- renderText({
      if ("3" %in% input$activities) {isolate(paste0("Fast walking: ",
                                                     datasetInput_walk_fst()$walk_fst, " min"))
      } else{
        ""
      }
    }
    )
  })



  # If "Slow running" is not selected: Show nothing
  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
    output$output13 <- renderText({
      if ("4" %in% input$activities) {isolate(paste0("Slow running: ",
                                                     datasetInput_run_slw()$run_slw, " min"))
      } else{
        ""
      }
    }
    )
  })

  # If "Moderate running" is not selected: Show nothing
  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
    output$output14 <- renderText({
      if ("5" %in% input$activities) {isolate(paste0("Moderate running: ",
                                                     datasetInput_run_mod()$run_mod, " min"))
      } else{
        ""
      }
    }
    )
  })


  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output10")
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output11")
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output12")
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output13")
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_i, {
    shinyjs::show("output14")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output10")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output11")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output12")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output13")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output14")
  })




  ###############################################################################
  # For cohort-level data - unstratified
  ###############################################################################


  calc_50_perc_b <- eventReactive(input$Calculate_b, {

    param_list <- c("avacc", "ig")

    perc50_frame <- data.frame(
      parameter = param_list
      , age = age_b()
      , perc50 = NA
    )

    for (i in seq_len(length(param_list))) {

      current_model <- model_list[[grep(paste0("centile", "_", param_list[i], "_b"), names(model_list))]]


      # Which centiles to plot
      # centiles_pred <- c(3, 15, 50, 85, 97)

      # Predict centiles and save them in a data frame
      cent50 <- centiles.pred(
        obj = current_model
        , type = "centiles"
        , xname = "Age"
        , xvalues = age_b() # For which x-values (age)
        , cent = c(50)
        , calibration = FALSE
      )

      perc50_frame$perc50[i] <- cent50[[2]]

    }

    perc50_frame

  })

#Slow walking

datasetInput_b <- reactive({
perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

tibble::tibble(
diff_avacc = input$avacc_b - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
diff_avacc_abs = abs(diff_avacc),
min_avacc_80 = 80 - input$avacc_b,
walk_slw = ifelse(perc_50_b()$percentile[1] < 50,
       round((1440*diff_avacc_abs)/min_avacc_80),
       round((1440*(input$avacc_b * 0.05))/min_avacc_80)),
digits=0)

})


observeEvent(c(input$avacc_b, input$Calculate_b), {
output$output1_b <- renderText({
isolate(paste0(datasetInput_b()$walk_slw, " min"))
})
})

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output1_b")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output1_b")
  })


  # Brisk walking

  datasetInput_2_b <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_b - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_b,
      walk_brsk = ifelse(perc_50_b()$percentile[1] < 50,
                         round((1440*diff_avacc_abs)/min_avacc_175),
                         round((1440*(input$avacc_b * 0.05))/min_avacc_175)),
      digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output2_b <- renderText({
    (paste0(datasetInput_2_b()$walk_brsk, " min"))
  })
})

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output2_b")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output2_b")
  })



  # Fast walking

  datasetInput_3_b <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

      tibble::tibble(
        diff_avacc = input$avacc_b - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
        diff_avacc_abs = abs(diff_avacc),
        min_avacc_400 = 400 - input$avacc_b,
        walk_fst =
      ifelse(perc_50_b()$percentile[1] < 50,
             round((1440*diff_avacc_abs)/min_avacc_400),
             round((1440*(input$avacc_b * 0.05))/min_avacc_400)),
      digits=0)
    })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output3_b <- renderText({
      isolate(paste0(datasetInput_3_b()$walk_fst, " min"))
  })
  })


  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output3_b")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output3_b")
  })


  # Slow running

  datasetInput_4_b <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

      tibble::tibble(
        diff_avacc = input$avacc_b - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
        diff_avacc_abs = abs(diff_avacc),
        min_avacc_750 = 750 - input$avacc_b,
        run_slw = ifelse(perc_50_b()$percentile[1] < 50,
             round((1440*diff_avacc_abs)/min_avacc_750),
             round((1440*(input$avacc_b * 0.05))/min_avacc_750)),
      digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output4_b <- renderText({
      isolate(paste0(datasetInput_4_b()$run_slw, " min"))
    })
  })


  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output4_b")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output4_b")
  })


  # Moderate running

  datasetInput_5_b <- reactive({

    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

      tibble::tibble(
        diff_avacc = input$avacc_b - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
        diff_avacc_abs = abs(diff_avacc),
        min_avacc_1000 = 1000 - input$avacc_b,
        run_mod = ifelse(perc_50_b()$percentile[1] < 50,
             round((1440*diff_avacc_abs)/min_avacc_1000),
             round((1440*(input$avacc_b * 0.05))/min_avacc_1000)),
      digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output5_b <- renderText({
    isolate(paste0(datasetInput_5_b()$run_mod, " min"))
  })
  })



  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output5_b")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output5_b")
  })



  # Customisable acceleration
  datasetInput_custom_b <- reactive({
    perc_50_res <- calc_50_perc_b() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_b - perc_50_res$perc50[perc_50_res$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_cust = custom_acc() - input$avacc_b,
      custom = ifelse(perc_50_b()$percentile[1] < 50,
           round((1440*diff_avacc_abs)/min_avacc_cust),
           round((1440*(input$avacc_b * 0.05))/min_avacc_cust)),
    digits=0)
  })

  observeEvent(c(input$avacc_i, input$Calculate_i), {
  output$output_cust_b <- renderText({
      paste0(datasetInput_custom_b()$custom, " min")
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_b, {
    shinyjs::show("output_cust_b")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output_cust_b")
  })



#===============================================================================
  # Combined activities - non-stratified data
#===============================================================================

  # Slow walking - combined
  datasetInput_walk_slw_b <- reactive({
    perc_50_res_b <- calc_50_perc_b() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_b - perc_50_res_b$perc50[perc_50_res_b$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - input$avacc_b,
      walk_slw = ifelse(perc_50_b()$percentile[1] < 50,
           round(0.5*(1440*diff_avacc_abs)/min_avacc_80),
           round(0.5*(1440*(input$avacc_b * 0.05))/min_avacc_80)),
    digits=0)
  })


  # Brisk walking
  datasetInput_walk_brsk_b <- reactive({
    perc_50_res_b <- calc_50_perc_b() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_b - perc_50_res_b$perc50[perc_50_res_b$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175- input$avacc_b,
      walk_brsk = ifelse(perc_50_b()$percentile[1] < 50,
           round(0.5*(1440*diff_avacc_abs)/min_avacc_175),
           round(0.5*(1440*(input$avacc_b * 0.05))/min_avacc_175)),
    digits=0)
  })

  # Fast walking
  datasetInput_walk_fst_b <- reactive({
    perc_50_res_b <- calc_50_perc_b() # Calculate 50. percentile
    # print(perc_50_res)

    tibble::tibble(
      diff_avacc = input$avacc_b - perc_50_res_b$perc50[perc_50_res_b$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_b,
      walk_fst = ifelse(perc_50_b()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_400),
                        round(0.5*(1440*(input$avacc_b * 0.05))/min_avacc_400)),
      digits=0)
  })

  # Slow running
  datasetInput_run_slw_b <- reactive({
    perc_50_res_b <- calc_50_perc_b() # Calculate 50. percentile

    # print(perc_50_res)

    tibble::tibble(
      diff_avacc = input$avacc_b - perc_50_res_b$perc50[perc_50_res_b$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750 - input$avacc_b,
      run_slw = ifelse(perc_50_b()$percentile[1] < 50,
                       round(0.5*(1440*diff_avacc_abs)/min_avacc_750),
                       round(0.5*(1440*(input$avacc_b * 0.05))/min_avacc_750)),
      digits=0)
  })

  # Moderate running
  datasetInput_run_mod_b <- reactive({
    perc_50_res_b <- calc_50_perc_b() # Calculate 50. percentile
    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_b - perc_50_res_b$perc50[perc_50_res_b$parameter %in% "avacc"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000 - input$avacc_b,
      run_mod = ifelse(perc_50_b()$percentile[1] < 50,
                       round(0.5(1440*diff_avacc_abs)/min_avacc_1000),
                       round(0.5(1440*(input$avacc_b * 0.05))/min_avacc_1000)),
      digits=0)
  })

  checkboxGroupInput("activities", "Variables:", c("Slow walking (3 kph)" = "1", "Brisk walking (5 kph)"="2", "Fast walking (6.5 kph)"="3", "Slow running (8 kph)"="4", "Moderate running (10 kph)"="5"))


  # If "Slow walking" is not selected: Show nothing
  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
  output$output10_b <- renderText({
    if ("1" %in% input$activities) {isolate(paste0("Slow walking: ",
                                                   datasetInput_walk_slw_b()$walk_slw, " min"))
    } else{
      ""
    }
  }
  )
  })

  # If "Brisk walking" is not selected: Show nothing
  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
  output$output11_b <- renderText({
    if ("2" %in% input$activities) {isolate(paste0("Brisk walking: ",
                                                   datasetInput_walk_brsk_b()$walk_brsk, " min"))
    } else{
      ""
    }
  }
  )
  })

  # If "Fast walking" is not selected: Show nothing
  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
  output$output12_b <- renderText({
    if ("3" %in% input$activities) {isolate(paste0("Fast walking: ",
                                                   datasetInput_walk_fst_b()$walk_fst, " min"))
    } else{
      ""
    }
  }
  )
  })

  # If "Slow running" is not selected: Show nothing
  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
  output$output13_b <- renderText({
    if ("4" %in% input$activities) {isolate(paste0("Slow running: ",
                                                   datasetInput_run_slw_b()$run_slw, " min"))
    } else{
      ""
    }
  }
  )
})
  # If "Moderate running" is not selected: Show nothing
  observe({
    # input$intensity # set inactive since the intensity slider is currently not used
  output$output14_b <- renderText({
    if ("5" %in% input$activities) {isolate(paste0("Moderate running: ",
                                                   datasetInput_run_mod_b()$run_mod, " min"))
    } else{
      ""
    }
  }
  )
  })


  #==========================================================================================
  # Present avacc and ig entered by the user in side panel
  #==========================================================================================

  # for individual-level data

  output$entered_avacc <- renderText({
    if(input$Calculate_i>0) {paste("Average acceleration = ", input$avacc_i, "mg")}
  })

  observeEvent(input$Calculate_i, {
    shinyjs::show("entered_avacc")
  })


  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("entered_avacc")
  })


  output$entered_ig <- renderText({
    if(input$Calculate_i>0) {paste("Intensity gradient =", input$ig_i)}
  })

  observeEvent(input$Calculate_i, {
    shinyjs::show("entered_ig")
  })


  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("entered_ig")
  })




  # Side panel code for stratified data

  output$entered_avacc_f <- renderText({
    if(input$Calculate_g>0) {paste("Females: ", "Average acceleration = ", input$avacc_f, "mg")}
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("entered_avacc_f")
  })


  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("entered_avacc_f")
  })


  output$entered_ig_f <- renderText({
    if(input$Calculate_g>0) {paste("Intensity gradient =", input$ig_f)}
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("entered_ig_f")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("entered_ig_f")
  })


  output$entered_avacc_m <- renderText({
    if(input$Calculate_g>0) {paste("Males:", "Average acceleration = ", input$avacc_m, "mg")}
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("entered_avacc_m")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("entered_avacc_m")
  })


  output$entered_ig_m <- renderText({
    if(input$Calculate_g>0) {paste("Intensity gradient =", input$ig_m)}
  })

  observeEvent(input$Calculate_g, {
    shinyjs::show("entered_ig_m")
  })


  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("entered_ig_m")
  })


  output$entered_avacc_b <- renderText({
    if(input$Calculate_b>0) {paste("Average acceleration = ", input$avacc_b, "mg")}
  })

  observeEvent(input$Calculate_b, {
    shinyjs::show("entered_avacc_b")
  })


  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("entered_avacc_b")
  })



  output$entered_ig_b <- renderText({
    if(input$Calculate_b>0) {paste("Intensity gradient = ", input$ig_b)}
  })

  observeEvent(input$Calculate_b, {
    shinyjs::show("entered_ig_b")
  })


  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("entered_ig_b")
  })


  ###############################################################################
  # Translation of results for cohort-level data - stratified
  ###############################################################################

  calc_50_perc_g <- eventReactive(input$Calculate_g, {

    param_list <- c("avacc", "ig")

    perc50_frame <- data.frame(
      parameter = rep(param_list, times = 2)
      , age = rep(c(age_m(), age_f()), each = 2)
      , sex = rep(c("m", "f"), each = 2)
      , perc50 = rep(NA, 4)
    )

    for (i in seq_len(length(param_list))) {

      current_model_m <- model_list[[grep(paste0("centile", "_", param_list[i], "_m"), names(model_list))]]
      current_model_f <- model_list[[grep(paste0("centile", "_", param_list[i], "_f"), names(model_list))]]


      # Which centiles to plot
      # centiles_pred <- c(3, 15, 50, 85, 97)

      # Predict centiles and save them in a data frame
      cent50_m <- centiles.pred(
        obj = current_model_m
        , type = "centiles"
        , xname = "Age"
        , xvalues = age_m() # For which x-values (age)
        , cent = c(50)
        , calibration = FALSE
      )

      cent50_f <- centiles.pred(
        obj = current_model_f
        , type = "centiles"
        , xname = "Age"
        , xvalues = age_f() # For which x-values (age)
        , cent = c(50)
        , calibration = FALSE
      )

      perc50_frame$perc50[perc50_frame$parameter %in% param_list[i] & perc50_frame$sex %in% "m"] <- cent50_m[[2]]
      perc50_frame$perc50[perc50_frame$parameter %in% param_list[i] & perc50_frame$sex %in% "f"] <- cent50_f[[2]]

    }

    perc50_frame

  })

  # Slow walking

  datasetInput_g_0_m <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - input$avacc_m,
      walk_slw = ifelse(perc_50_g()$percentile[1] < 50,
                        round((1440*diff_avacc_abs)/min_avacc_80),
                        round((1440*(input$avacc_m * 0.05))/min_avacc_80)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output1_m <- renderText({
      isolate(paste0("Males: ",
                     datasetInput_g_0_m()$walk_slw, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output1_m")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output1_m")
  })


  datasetInput_g_0_f <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - input$avacc_f,
      walk_slw = ifelse(perc_50_g()$percentile[1] < 50,
                        round((1440*diff_avacc_abs)/min_avacc_80),
                        round((1440*(input$avacc_f * 0.05))/min_avacc_80)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output1_f <- renderText({
      isolate(paste0("Females: ",
                     datasetInput_g_0_f()$walk_slw, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output1_f")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output1_f")
  })


  # Brisk walking

  datasetInput_g_1_m <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_m,
      walk_brsk = ifelse(perc_50_g()$percentile[1] < 50,
                        round((1440*diff_avacc_abs)/min_avacc_175),
                        round((1440*(input$avacc_m * 0.05))/min_avacc_175)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output2_m <- renderText({
      isolate(paste0("Males: ",
                     datasetInput_g_1_m()$walk_brsk, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output2_m")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output2_m")
  })



  datasetInput_g_1_f <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_f,
      walk_brsk = ifelse(perc_50_g()$percentile[1] < 50,
                        round((1440*diff_avacc_abs)/min_avacc_175),
                        round((1440*(input$avacc_f * 0.05))/min_avacc_175)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output2_f <- renderText({
      isolate(paste0("Females: ",
                     datasetInput_g_1_f()$walk_brsk, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output2_f")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output2_f")
  })



  # Fast walking

  datasetInput_g_2_m <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_m,
      walk_fst = ifelse(perc_50_g()$percentile[1] < 50,
                         round((1440*diff_avacc_abs)/min_avacc_400),
                         round((1440*(input$avacc_m * 0.05))/min_avacc_400)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output3_m <- renderText({
      isolate(paste0("Males: ",
                     datasetInput_g_2_m()$walk_fst, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output3_m")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output3_m")
  })


  datasetInput_g_2_f <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_f,
      walk_fst = ifelse(perc_50_g()$percentile[1] < 50,
                         round((1440*diff_avacc_abs)/min_avacc_400),
                         round((1440*(input$avacc_f * 0.05))/min_avacc_400)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output3_f <- renderText({
      isolate(paste0("Females: ",
                     datasetInput_g_2_f()$walk_fst, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output3_f")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output3_f")
  })



  # Slow running

  datasetInput_g_3_m <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750 - input$avacc_m,
      run_slw = ifelse(perc_50_g()$percentile[1] < 50,
                        round((1440*diff_avacc_abs)/min_avacc_750),
                        round((1440*(input$avacc_m * 0.05))/min_avacc_750)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output4_m <- renderText({
      isolate(paste0("Males: ",
                     datasetInput_g_3_m()$run_slw, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output4_m")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output4_m")
  })



  datasetInput_g_3_f <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750 - input$avacc_f,
      run_slw = ifelse(perc_50_g()$percentile[1] < 50,
                         round((1440*diff_avacc_abs)/min_avacc_750),
                         round((1440*(input$avacc_f * 0.05))/min_avacc_750)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output4_f <- renderText({
      isolate(paste0("Females: ",
                     datasetInput_g_3_f()$run_slw, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output4_f")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output4_f")
  })



  # Moderate running

  datasetInput_g_4_m <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000 - input$avacc_m,
      run_mod = ifelse(perc_50_g()$percentile[1] < 50,
                       round((1440*diff_avacc_abs)/min_avacc_1000),
                       round((1440*(input$avacc_m * 0.05))/min_avacc_1000)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output5_m <- renderText({
      isolate(paste0("Males: ",
                     datasetInput_g_4_m()$run_mod, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output5_m")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output5_m")
  })


  datasetInput_g_4_f <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000 - input$avacc_f,
      run_mod = ifelse(perc_50_g()$percentile[1] < 50,
                         round((1440*diff_avacc_abs)/min_avacc_1000),
                         round((1440*(input$avacc_f * 0.05))/min_avacc_1000)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output5_f <- renderText({
      isolate(paste0("Females: ",
                     datasetInput_g_4_f()$run_mod, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output5_f")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output5_f")
  })



  # Customisable acceleration


  datasetInput_custom_f <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_cust = custom_acc() - input$avacc_f,
      custom = round((1440*diff_avacc_abs)/min_avacc_cust), digits=0)

  })

  datasetInput_custom_f <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_cust = custom_acc() - input$avacc_f,
      custom = ifelse(perc_50_g()$percentile[1] < 50,
                       round((1440*diff_avacc_abs)/min_avacc_cust),
                       round((1440*(input$avacc_f * 0.05))/min_avacc_cust)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output_cust_f <- renderText({
      isolate(paste0("Females: ",
                     datasetInput_custom_f()$custom, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output_cust_f")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output_cust_f")
  })



  # Customisable acceleration

  datasetInput_custom_m <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_cust = custom_acc() - input$avacc_m,
      custom = round((1440*diff_avacc_abs)/min_avacc_cust), digits=0)

  })

  datasetInput_custom_m <- reactive({

    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_cust = custom_acc() - input$avacc_m,
      custom = ifelse(perc_50_g()$percentile[1] < 50,
                       round((1440*diff_avacc_abs)/min_avacc_cust),
                       round((1440*(input$avacc_m * 0.05))/min_avacc_cust)),
      digits=0)

  })

  observeEvent(c(input$avacc_g, input$Calculate_g), {
    output$output_cust_m <- renderText({
      isolate(paste0("Males: ",
                     datasetInput_custom_m()$custom, " min"))
    })
  })

  # show output if Calculate_i button is pressed
  observeEvent(input$Calculate_g, {
    shinyjs::show("output_cust_m")
  })

  # hide output if reset button is pressed
  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output_cust_m")
  })



  # Combined recommendations

  # Slow walking - combined for males

  datasetInput_walk_slw_m <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - input$avacc_m,
      walk_slw = ifelse(perc_50_g()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_80),
                        round(0.5*(1440*(input$avacc_m * 0.05))/min_avacc_80)),
      digits=0)
  })


  # Brisk walking

  datasetInput_walk_brsk_m <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_m,
      walk_brsk = ifelse(perc_50_g()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_175),
                        round(0.5*(1440*(input$avacc_m * 0.05))/min_avacc_175)),
      digits=0)
  })


  # Fast walking

  datasetInput_walk_fst_m <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_m,
      walk_fst = ifelse(perc_50_g()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_400),
                        round(0.5*(1440*(input$avacc_m * 0.05))/min_avacc_400)),
      digits=0)
  })

  # Slow running
  datasetInput_run_slw_m <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750 - input$avacc_m,
      run_slw = ifelse(perc_50_g()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_750),
                        round(0.5*(1440*(input$avacc_m * 0.05))/min_avacc_750)),
      digits=0)
  })

  # Moderate running

  datasetInput_run_mod_m <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_m - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "m"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000 - input$avacc_m,
      run_mod = ifelse(perc_50_g()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_1000),
                        round(0.5*(1440*(input$avacc_m * 0.05))/min_avacc_1000)),
      digits=0)
  })


  # If "Slow walking" is not selected: Show nothing
  output$output10_m <- renderText({
    if ("1" %in% input$activities) {isolate(paste0("Slow walking (males): ",
                                                   datasetInput_walk_slw_m()$walk_slw, " min"))
    } else{
      ""
    }
  }
  )

  observeEvent(input$Calculate_g, {
    shinyjs::show("output10_m")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output10_m")
  })

  # If "Brisk walking" is not selected: Show nothing
  output$output11_m <- renderText({
    if ("2" %in% input$activities) {isolate(paste0("Brisk walking (males): ",
                                                   datasetInput_walk_brsk_m()$walk_brsk, " min"))
    } else{
      ""
    }
  }
  )

  observeEvent(input$Calculate_g, {
    shinyjs::show("output11_m")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output11_m")
  })

  # If "Fast walking" is not selected: Show nothing
  output$output12_m <- renderText({
    if ("3" %in% input$activities) {isolate(paste0("Fast walking (males): ",
                                                   datasetInput_walk_fst_m()$walk_fst, " min"))
    } else{
      ""
    }
  }
  )


  observeEvent(input$Calculate_g, {
    shinyjs::show("output12_m")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output12_m")
  })

  # If "Slow running" is not selected: Show nothing
  output$output13_m <- renderText({
    if ("4" %in% input$activities) {isolate(paste0("Slow running (males): ",
                                                   datasetInput_run_slw_m()$run_slw, " min"))
    } else{
      ""
    }
  }
  )

  observeEvent(input$Calculate_g, {
    shinyjs::show("output13_m")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output13_m")
  })

  # If "Moderate running" is not selected: Show nothing
  output$output14_m <- renderText({
    if ("5" %in% input$activities) {isolate(paste0("Moderate running (males): ",
                                                   datasetInput_run_mod_m()$run_mod, " min"))
    } else{
      ""
    }
  }
  )

  observeEvent(input$Calculate_g, {
    shinyjs::show("output14_m")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output14_m")
  })


  # Slow walking - combined for females

  datasetInput_walk_slw_f <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_80 = 80 - input$avacc_f,
      walk_slw = ifelse(perc_50_g()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_80),
                        round(0.5*(1440*(input$avacc_f * 0.05))/min_avacc_80)),
      digits=0)
  })

  # Brisk walking

  datasetInput_walk_brsk_f <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_175 = 175 - input$avacc_f,
      walk_brsk = ifelse(perc_50_g()$percentile[1] < 50,
                         round(0.5*(1440*diff_avacc_abs)/min_avacc_175),
                         round(0.5*(1440*(input$avacc_f * 0.05))/min_avacc_175)),
      digits=0)
  })


  # Fast walking

  datasetInput_walk_fst_f <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_400 = 400 - input$avacc_f,
      walk_fst = ifelse(perc_50_g()$percentile[1] < 50,
                        round(0.5*(1440*diff_avacc_abs)/min_avacc_400),
                        round(0.5*(1440*(input$avacc_f * 0.05))/min_avacc_400)),
      digits=0)
  })

  # Slow running

  datasetInput_run_slw_f <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_750 = 750 - input$avacc_f,
      run_slw = ifelse(perc_50_g()$percentile[1] < 50,
                       round(0.5*(1440*diff_avacc_abs)/min_avacc_750),
                       round(0.5*(1440*(input$avacc_f * 0.05))/min_avacc_750)),
      digits=0)
  })

  # Moderate running

  datasetInput_run_mod_f <- reactive({
    perc_50_res <- calc_50_perc_g() # Calculate 50. percentile

    # print(perc_50_res)
    tibble::tibble(
      diff_avacc = input$avacc_f - perc_50_res$perc50[perc_50_res$parameter %in% "avacc" & perc_50_res$sex %in% "f"],
      diff_avacc_abs = abs(diff_avacc),
      min_avacc_1000 = 1000 - input$avacc_f,
      run_mod = ifelse(perc_50_g()$percentile[1] < 50,
                       round(0.5*(1440*diff_avacc_abs)/min_avacc_1000),
                       round(0.5*(1440*(input$avacc_f * 0.05))/min_avacc_1000)),
      digits=0)
  })


  # If "Slow walking" is not selected: Show nothing
  output$output10_f <- renderText({
    if ("1" %in% input$activities) {isolate(paste0("Slow walking (females): ",
                                                   datasetInput_walk_slw_f()$walk_slw, " min"))
    } else{
      ""
    }
  }
  )


  observeEvent(input$Calculate_g, {
    shinyjs::show("output10_f")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output10_f")
  })

  # If "Brisk walking" is not selected: Show nothing
  output$output11_f <- renderText({
    if ("2" %in% input$activities) {isolate(paste0("Brisk walking (females): ",
                                                   datasetInput_walk_brsk_f()$walk_brsk, " min"))
    } else{
      ""
    }
  }
  )

  observeEvent(input$Calculate_g, {
    shinyjs::show("output11_f")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output11_f")
  })

  # If "Fast walking" is not selected: Show nothing
  output$output12_f <- renderText({
    if ("3" %in% input$activities) {isolate(paste0("Fast walking (females): ",
                                                   datasetInput_walk_fst_f()$walk_fst, " min"))
    } else{
      ""
    }
  }
  )


  observeEvent(input$Calculate_g, {
    shinyjs::show("output12_f")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output12_f")
  })

  # If "Slow running" is not selected: Show nothing
  output$output13_f <- renderText({
    if ("4" %in% input$activities) {isolate(paste0("Slow running (females): ",
                                                   datasetInput_run_slw_f()$run_slw, " min"))
    } else{
      ""
    }
  }
  )


  observeEvent(input$Calculate_g, {
    shinyjs::show("output13_f")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output13_f")
  })

  # If "Moderate running" is not selected: Show nothing
  output$output14_f <- renderText({
    if ("5" %in% input$activities) {isolate(paste0("Moderate running (females): ",
                                                   datasetInput_run_mod_f()$run_mod, " min"))
    } else{
      ""
    }
  }
  )


  observeEvent(input$Calculate_g, {
    shinyjs::show("output14_f")
  })

  observeEvent(input$reset | input$reset1 | input$reset2, {
    shinyjs::hide("output14_f")
  })




  ################################################################################
  # Prepare downloadable pdf file
  ################################################################################

  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "interpretablePA_report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list() # insert parameters here

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}





#------------------------------------------------------------------------------------------
# Application
#------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)

}
