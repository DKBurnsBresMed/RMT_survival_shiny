

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

