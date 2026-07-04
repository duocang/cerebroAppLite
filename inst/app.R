##----------------------------------------------------------------------------##
## load packages
##----------------------------------------------------------------------------##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)

##----------------------------------------------------------------------------##
## set options
##----------------------------------------------------------------------------##
custom_welcome_message <- "Welcome to Cerebro! This is a custom welcome message. You can change it in the app options."
Cerebro.options <<- list(
  "mode" = "closed",
  ## This bundled app uses the single example data set as a lightweight,
  ## fast-starting default (it also serves as the package test fixture).
  ##
  ## To explore the *multi-dataset* switcher with the three demo samples
  ## (PBMC Full / Healthy / B-cell rich), launch via createShinyApp() with a
  ## named vector instead -- see vignette("multi_crb"). Example:
  ##   createShinyApp(cerebro_data = c(
  ##     "PBMC - Full (T+B)"     = system.file("extdata/v1.4/demo_full_tcr_bcr.crb", package = "cerebroAppLite"),
  ##     "PBMC - Healthy (T/NK)" = system.file("extdata/v1.4/demo_healthy_t.crb",    package = "cerebroAppLite"),
  ##     "PBMC - B-cell rich"    = system.file("extdata/v1.4/demo_bcell_rich.crb",   package = "cerebroAppLite")
  ##   ))
  "crb_file_to_load" = "extdata/v1.4/example.crb",
  "expression_matrix_mode" = "h5",
  "expression_matrix_h5" = "extdata/v1.4/example.h5",
  "cerebro_root" = ".",
  "welcome_message" = custom_welcome_message,
  "overview_default_point_size" = 1,
  "gene_expression_default_point_size" = 2,
  "overview_default_point_opacity" = 0.3,
  "gene_expression_default_point_opacity" = 0.5,
  "overview_default_percentage_cells_to_show" = 100,
  "gene_expression_default_percentage_cells_to_show" = 20,
  "projections_show_hover_info" = FALSE
)

options(shiny.maxRequestSize = 800 * 1024^2)

##----------------------------------------------------------------------------##
## load server and UI functions
##----------------------------------------------------------------------------##
source("shiny/v1.4/shiny_UI.R", local = TRUE)
source("shiny/v1.4/shiny_server.R", local = TRUE)

##----------------------------------------------------------------------------##
## launch app
##----------------------------------------------------------------------------##
shiny::shinyApp(ui = ui, server = server)
