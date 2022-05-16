

# Library -----------------------------------------------------------------


library(shiny)
library(shinyWidgets)
library(bslib)
library(survival)
library(flexsurv)
library(survminer)
library(tidyverse)
library(data.table)
library(DT)


source("./functions/f_master.R")


# Default values ----------------------------------------------------------



# Table stuff -------------------------------------------------------------

Config_UI_Res_BDTabopts <- list(
  lengthChange = FALSE,
  paging = FALSE,
  searching = FALSE,
  info = FALSE,
  ordering = TRUE,
  compact = TRUE,
  scrollX = TRUE,
  dom = 'Bfrtip',
  buttons = list(
    'colvis',
    'print',
    list(
      extend = 'collection',
      text = 'Download',
      buttons = list('copy', 'csv', 'excel', 'pdf')
    )
  ),
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
    "}"
  )
)




# Graphical stuff ---------------------------------------------------------

Config_graph_SurvlineColours <- c(
  "KM"           = alpha("black", 1),
  "exp"          = alpha("#DB3B93", 0.8),
  "weibull"      = alpha("#45B9D1", 0.8),
  "lnorm"        = alpha("#002D5C", 0.8),
  "llogis"       = alpha("#F9161C", 0.8),
  "gompertz"     = alpha("#B97E22", 0.8),
  "gengamma"     = alpha("#75AC3F", 0.8),
  "gamma"        = alpha("#1AA393", 0.8))

Config_graph_SurvLineThickness <- c(
  "KM"           = 1.5,
  "exp"          = 0.6,
  "weibull"      = 0.6,
  "lnorm"        = 0.6,
  "llogis"       = 0.6,
  "gompertz"     = 0.6,
  "gengamma"     = 0.6,
  "gamma"        = 0.6)

