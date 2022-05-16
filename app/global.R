

# Library -----------------------------------------------------------------


library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(bslib)
library(survival)
library(flexsurv)
library(survminer)
library(tidyverse)
library(data.table)
library(DT)


source("./functions/f_master.R")


# Default values ----------------------------------------------------------


# labels for distributions for regression analysis
dists     <- c("exponential","weibull","llogis",      "lnorm",     "gompertz","gengamma")
DistNames <- c("Exponential","Weibull","Log-logistic","Log-normal","Gompertz","Generalised gamma")
names(dists) <- dists
names(DistNames) <- DistNames


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
  "KM"           = "black",
  "exp"          = "#DB3B93",
  "weibull"      = "#45B9D1",
  "lnorm"        = "#002D5C",
  "llogis"       = "#F9161C",
  "gompertz"     = "#B97E22",
  "gengamma"     = "#75AC3F",
  "gamma"        = "#1AA393"
)

Config_graph_SurvLineThickness <- c(
  "KM"           = 1.5,
  "exp"          = 0.6,
  "weibull"      = 0.6,
  "lnorm"        = 0.6,
  "llogis"       = 0.6,
  "gompertz"     = 0.6,
  "gengamma"     = 0.6,
  "gamma"        = 0.6)

