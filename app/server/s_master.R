#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # raw_data loading from csv file:
  
  raw_data <- reactive({
    req(!is.null(input$csv_file_location))
    
    req(file.exists(input$csv_file_location$datapath))

    
    path_to_file <- input$csv_file_location$datapath
    top_row_labs <- input$csv_top_row_labs
    
    as.data.frame(fread(
      file = path_to_file,
      header = top_row_labs
    ))
  })
  output$ui_raw_csv_location <- renderPrint({print(input$csv_file_location$name)})
  output$ui_raw_csv_output <- renderDataTable({
    datatable(
      data = raw_data(),
      rownames = FALSE,
      extensions = 'Buttons',
      options = Config_UI_Res_BDTabopts
    )
  })
  
  
  # Selection of time and event variables:
  
  raw_data_cols <- reactive({
    req(!is.null(raw_data()))
    colnames(raw_data())
  })
  
  
  # we need to simultaneously determine t and e without replacement. renderUI
  # is good for this as we can validate the data before it goes into the UI 
  # elements
  
  output$ui_t_e_selector <- renderUI({
    req(!is.null(raw_data_cols()))
    
    # the two variables are called time and event. We can do some logical stuff
    # based on selections:
    
    if (is.null(input$time)) {
      sel_time <- raw_data_cols()[1]
    } else {
      sel_time <- input$time
    }
    
    if (is.null(input$event)) {
      sel_event <- raw_data_cols()[2]
    } else {
      sel_event <- input$event
    }
    
    # now, if they're not null and theyr'e the same, then make event different
    # from time so they don't clash.
    if (all(
      !is.null(input$time),
      !is.null(input$event),
      input$time == input$event
    )) {
      available_options <- which(!raw_data_cols() %in% sel_time)
      sel_event <- available_options[1]
    }
    
    # now generate the dropdowns:
    
    tagList(
      fluidRow(
        width = 12,
        column(
          6,
          pickerInput(
            inputId = "time",
            label = "Time variable",
            choices = raw_data_cols()[which(!raw_data_cols() %in% sel_event)],
            selected = sel_time,
            multiple = FALSE,
            width = "100%"
          )
        ),
        column(
          6,
          pickerInput(
            inputId = "event",
            label = "Event variable",
            choices = raw_data_cols()[which(!raw_data_cols() %in% sel_time)],
            selected = sel_event,
            multiple = FALSE,
            width = "100%"
          )
        )
      ),
      fluidRow(
        width = 12,
        column(
          12,
          pickerInput(
            inputId = "covariates",
            label = "Continuous covariates",
            choices = raw_data_cols()[which(!raw_data_cols() %in% c(sel_time, sel_event))],
            selected = NULL,
            multiple = TRUE,
            width = "100%"
          )
        )
      ),
      fluidRow(
        width = 12,
        column(
          12,
          pickerInput(
            inputId = "factors",
            label = "Factor variables",
            choices = raw_data_cols()[which(!raw_data_cols() %in% c(sel_time, sel_event))],
            selected = NULL,
            multiple = TRUE,
            width = "100%"
          )
        )
      )
    )
    
    
  })
  
  # Print out time, event and covariates
  output$print_time       <- renderPrint({print(input$time)})
  output$print_event      <- renderPrint({print(input$event)})
  output$print_covariates <- renderPrint({print(input$covariates)})
  output$print_factors    <- renderPrint({print(input$factors)})
  
  
  # Survival regression formula:
  
  surv_formula <- eventReactive(input$generate_formula, {
    
    req(!is.null(input$time))
    req(!is.null(input$event))
    
    return(
      SurvregFormulaGen(
        t           = raw_data()[,input$time],
        e           = raw_data()[,input$event],
        covs        = raw_data()[,input$covariates],
        factors     = raw_data()[,input$factors],
        nam_t       = input$time,
        nam_e       = input$event,
        nam_covs    = input$covariates,
        nam_factors = input$factors,
        DEBUG       = TRUE
      )
    )
    
  })
  
  output$print_surv_formula <- renderPrint({print(surv_formula())})
  
  
  
  # Run regression analysis
  
  flexsurv_regressions <- eventReactive(input$run_regressions,{
    
    # run regression analysis
    dists     <- c("exponential","weibull","llogis",      "lnorm",     "gompertz","gengamma")
    DistNames <- c("exponential","weibull","log_logistic","log_normal","gompertz","gen_gamma")
    names(dists) <- dists
    names(DistNames) <- DistNames
    
    return(
      lapply(dists, function(model){
        flexsurvreg(surv_formula(), data = raw_data(), dist = dists[model])
      })
    )
  })
  
  output$print_flexsurv_regressions <- renderPrint({print(flexsurv_regressions())})
  
  
  
}
