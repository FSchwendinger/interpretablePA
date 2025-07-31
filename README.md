Interpreting cut-point-free accelerometer data using interpretablePA
================

- [Project description](#project-description)
  - [Target audience](#target-audience)
  - [Practical application](#practical-application)
  - [Reference populations](#reference-populations)
- [Installation](#installation)
- [Applications and Usage](#applications-and-usage)
  - [1. `interpret.pa()` – Shiny app for adult
    data](#1-interpretpa--shiny-app-for-adult-data)
  - [Examples](#examples)
  - [2. `interpret.pa.children()` – Function for children’s
    data](#2-interpretpachildren--function-for-childrens-data)
- [References](#references)
- [Contact](#contact)
- [Session info](#session-info)
- [License](#license)

<p align="right">

<img src="inst/img/app_logo.png" align="right" width="120"/>

</p>

[![DOI](https://img.shields.io/badge/DOI-10.1101%2023.04.19.23288786-orange)](https://doi.org/10.1101/2023.04.19.23288786)

## Project description

### Target audience

`interpretablePA` was created for researchers and clinicians interested
in measuring human movement using raw-acceleration accelerometers in
various populations or individuals.

### Practical application

This package supports the use of cut-point-free accelerometer metrics,
namely daily average acceleration (AvAcc) and intensity gradient (IG),
to assess physical activity. AvAcc and IG have been shown to be a viable
alternative to traditional metrics and are strongly related to various
health outcomes.

### Reference populations

The package provides:

- A Shiny application for interpreting adult data, based on reference
  values from a Swiss population (ages 20–89)

- A function for interpreting children’s data, using reference values
  from an English youth cohort

`interpretablePA` requires data processed using the R-package GGIR in a
similar manner. GGIR supports the processing of multi-day raw
accelerometer data for physical activity and sleep research. See the
GGIR package
[vignette](https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html)
for further information.

## Installation

You can install the package from GitHub by typing the following:

``` r
install.packages("remotes")

remotes::install_github("FSchwendinger/interpretablePA")
```

## Applications and Usage

### 1. `interpret.pa()` – Shiny app for adult data

The main entry point is the `interpret.pa()` function, which launches a
Shiny application to interactively interpret accelerometer data in
adults.

Typical use:

``` r
interpret.pa()
```

### Examples

The below images will give you some insights into the package. A typical
workflow could be:

1.  The user run installs and loads `interpretablePA` and runs
    `interpret.pa()`. This starts the application.

2.  The user decides what data format they want to enter (data of an
    individual, pooled means/medians of a whole study cohort that are
    stratified by sex or not, or data of several individuals with sample
    size = N) and selects the respective option under panel “*1) User
    data*”.

3.  Assuming the user decides to enter data of one individual (see
    Figure 1), they would fill in all the fields, i.e. sex, age, height,
    body weight, average acceleration, and intensity gradient; then
    press “Calculate”.

    **Figure 1.** One of three data entry options the user can utilize.

    ![](images/interpretablePA_1.png "Individual-level data entry option.")

4.  Panels “2) View results” (see Figure 2) and “3) Translation of
    results” (see Figure 3) are now accessible.

5.  The user can find out the exact percentile the individual is on
    compared to our reference values and download centile plots.

    **Figure 2.** Example of graphical output. Green dots are the data
    entered by the user.

    ![](images/interpretablePA_2.png)

6.  In panel “3) Translation of results”, the user is provided with
    information on a) what is necessary for the individual to reach the
    50th percentile or increase their physical activity by 5%, b) which
    changes would be needed to achieve a clinically relevant improvement
    in cardiorespiratory fitness, and c) how much more physical activity
    would need to be performed to reduce the risk of death and disease.

**Figure 3.** Example of the translation of results.

![](images/interpretablePA_3.png)

### 2. `interpret.pa.children()` – Function for children’s data

The function `interpret.pa.children()` allows automated batch
interpretation of **children’s** AvAcc and IG values against age- and
sex-specific reference centiles.

``` r
interpret.pa.children(
  dat_path = "child_characteristics.csv",
  part2_path = "part2_summary.csv",
  output_path = "children_results.csv",
  col_id = "ID",
  col_sex = "sex",
  col_age = "age",
  sex_code_male = "0",
  sex_code_female = "1"
)
```

## References

Please cite the appropriate reference depending on which part of the
package you use:

- If you use the **Shiny application** for **adult reference values**
  (`interpret.pa()`), cite:

> **Schwendinger F., Wagner J., Knaier R., Infanger D., Rowlands A.V.,
> Hinrichs T., & Schmidt-Trucksaess A. (2024).**  
> *Accelerometer Metrics: Healthy Adult Reference Values, Associations
> with Cardiorespiratory Fitness, and Clinical Implications.*  
> Medicine and Science in Sports and Exercise, 56(2), 170–180.  
> <https://doi.org/10.1249/MSS.0000000000003299>

- If you use the **function for children’s data**
  (`interpret.pa.children()`), cite **also**:

> **Fairclough S.J., Noonan R.J., Rowlands A.V., Boddy L.M., Davies
> I.G., Ridgers N.D., Mackintosh K.A., & Knowles Z.R. (2023).**  
> *Centile curves for accelerometer‐assessed physical activity and
> sedentary time in English children.*  
> International Journal of Behavioral Nutrition and Physical Activity,
> 20(1), 1–12.  
> <https://doi.org/10.1186/s12966-023-01435-z>

## Contact

If you are interested in contributing or have any queries regarding this
package, feel free to reach out to:

[Fabian
Schwendinger](https://dsbg.unibas.ch/de/personen/fabian-schwendinger/)

## Session info

    ## R version 4.5.0 (2025-04-11 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26100)
    ## 
    ## Matrix products: default
    ##   LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/Zurich
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] parallel  splines   stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] interpretablePA_0.0.0.9000 readxl_1.4.5              
    ##  [3] directlabels_2025.5.20     DT_0.33                   
    ##  [5] shinyjs_2.1.0              shinyBS_0.61.1            
    ##  [7] shinybusy_0.3.3            fontawesome_0.5.3         
    ##  [9] ggpubr_0.6.0               scales_1.4.0              
    ## [11] rms_8.0-0                  Hmisc_5.2-3               
    ## [13] gamlss_5.4-22              nlme_3.1-168              
    ## [15] gamlss.dist_6.1-1          gamlss.data_6.0-6         
    ## [17] shinyalert_3.1.0           shinythemes_1.2.0         
    ## [19] palmerpenguins_0.1.1       shiny_1.10.0              
    ## [21] lubridate_1.9.4            forcats_1.0.0             
    ## [23] stringr_1.5.1              dplyr_1.1.4               
    ## [25] purrr_1.0.4                readr_2.1.5               
    ## [27] tidyr_1.3.1                tibble_3.3.0              
    ## [29] ggplot2_3.5.2              tidyverse_2.0.0           
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gridExtra_2.3      sandwich_3.1-1     rlang_1.1.6       
    ##  [4] magrittr_2.0.3     multcomp_1.4-28    polspline_1.1.25  
    ##  [7] compiler_4.5.0     vctrs_0.6.5        quantreg_6.1      
    ## [10] quadprog_1.5-8     pkgconfig_2.0.3    fastmap_1.2.0     
    ## [13] backports_1.5.0    utf8_1.2.6         promises_1.3.3    
    ## [16] rmarkdown_2.29     tzdb_0.5.0         MatrixModels_0.5-4
    ## [19] xfun_0.52          jsonlite_2.0.0     later_1.4.2       
    ## [22] broom_1.0.8        cluster_2.1.8.1    R6_2.6.1          
    ## [25] stringi_1.8.7      RColorBrewer_1.1-3 car_3.1-3         
    ## [28] rpart_4.1.24       cellranger_1.1.0   Rcpp_1.0.14       
    ## [31] knitr_1.50         zoo_1.8-14         base64enc_0.1-3   
    ## [34] httpuv_1.6.16      Matrix_1.7-3       nnet_7.3-20       
    ## [37] timechange_0.3.0   tidyselect_1.2.1   yaml_2.3.10       
    ## [40] rstudioapi_0.17.1  abind_1.4-8        codetools_0.2-20  
    ## [43] lattice_0.22-6     withr_3.0.2        evaluate_1.0.4    
    ## [46] foreign_0.8-90     survival_3.8-3     pillar_1.10.2     
    ## [49] rsconnect_1.5.0    carData_3.0-5      checkmate_2.3.2   
    ## [52] generics_0.1.4     hms_1.1.3          xtable_1.8-4      
    ## [55] glue_1.8.0         tools_4.5.0        data.table_1.17.6 
    ## [58] SparseM_1.84-2     ggsignif_0.6.4     mvtnorm_1.3-3     
    ## [61] grid_4.5.0         colorspace_2.1-1   htmlTable_2.4.3   
    ## [64] Formula_1.2-5      cli_3.6.5          gtable_0.3.6      
    ## [67] rstatix_0.7.2      digest_0.6.37      TH.data_1.1-3     
    ## [70] htmlwidgets_1.6.4  farver_2.1.2       htmltools_0.5.8.1 
    ## [73] lifecycle_1.0.4    mime_0.13          MASS_7.3-65

## License

[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-orange.svg)](https://www.gnu.org/licenses/gpl-3.0)
