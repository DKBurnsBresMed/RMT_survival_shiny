#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

print(getwd())

# Define UI for application that draws a histogram
ui <- navbarPage(
  title       = "Survival analysis",
  id          = "tab_main",
  selected    = "data",
  fluid       = TRUE,
  theme       = "flatly",
  windowTitle = "Survival analysis",
  lang        = "en-GB",
  tabPanel(
    title = "Load in csv file",
    value = "data",
    icon = icon("upload"),
    h2("Load data:"),
    fileInput(
      inputId = "csv_file_location",
      label = "Please load csv file",
      multiple = FALSE,
      accept = ".csv",
      width = "100%",
      buttonLabel = "Load .csv",
      placeholder = "Please load before continuing"
    ),
    checkboxInput(
      inputId = "csv_top_row_labs",
      label = "Include top row as labels?",
      value = TRUE
    ),
    hr(),
    h2("File name:"),
    verbatimTextOutput("ui_raw_csv_location"),
    hr(),
    h2("Raw data:"),
    dataTableOutput("ui_raw_csv_output")
  ),
  tabPanel(
    title = "Select time and event",
    value = "t_and_e",
    icon = icon("hand-pointer"),
    h2("Select time, event, and covariates"),
    uiOutput("ui_t_e_selector"),
    hr(),
    h2("Equation"),
    actionBttn(
      inputId = "generate_formula",
      label = "Generate equation for regression models",
      icon = icon("cogs"),
      style = "unite",
      color = "success",
      size = "lg",
      block = TRUE
    ),
    verbatimTextOutput("print_surv_formula"),
    hr(),
    h2("QC prints"),
    fluidRow(
      width = 12,
      column(2,h2("Time:")),column(10,verbatimTextOutput("print_time"))
    ),
    fluidRow(
      width = 12,
      column(2,h2("Event")),column(10,verbatimTextOutput("print_event"))
    ),
    fluidRow(
      width = 12,
      column(2,h2("Continuous")),column(10,verbatimTextOutput("print_covariates"))
    ),
    fluidRow(
      width = 12,
      column(2,h2("Factor")),column(10,verbatimTextOutput("print_factors"))
    )
  ),
  tabPanel(
    title = "Perform regression",
    value = "reg",
    icon = icon("cogs"),
    actionBttn(
      inputId = "run_regressions",
      label = "Run regression analysis",
      icon = icon("cogs"),
      style = "unite",
      color = "success",
      size = "lg",
      block = TRUE
    ),
    verbatimTextOutput("print_flexsurv_regressions")
  )
)
