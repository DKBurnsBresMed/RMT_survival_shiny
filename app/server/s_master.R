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
  
  surv_fit <- eventReactive(input$generate_formula, {
    
    req(!is.null(input$time))
    req(!is.null(input$event))
    
    out_list <- list()
    
    out_list$df <- raw_data()
    
    out_list$txt <- list(
      time  = input$time,
      event = input$event,
      cov   = input$covariates,
      fac   = input$factors
    )
    
    out_list$form <- SurvregFormulaGen(
      t           = out_list$df[,out_list$txt$time],
      e           = out_list$df[,out_list$txt$event],
      covs        = out_list$df[,out_list$txt$cov],
      factors     = out_list$df[,out_list$txt$fac],
      nam_t       = out_list$txt$time,
      nam_e       = out_list$txt$event,
      nam_covs    = out_list$txt$cov,
      nam_factors = out_list$txt$fac,
      DEBUG       = TRUE
    )
    out_list$survfit <- survfit(form)
    
    # dataframe for survminer:
    
    out_list$sm_df <- Func_MakeDatSurvFriendly(
      Data_required = out_list$df,
      time_column = out_list$txt$time,
      event_column = out_list$txt$event,
      t_multiplier = 1
    )
    
    out_list$est <- Func_GetSurvEst(Data_required = out_list$sm_df)
    out_list$plot <- Func_MakeKMPlotForUI(
      SurvEstimate  = out_list$est,
      Data_required = out_list$sm_df,
      censor_ticks  = TRUE,
      conf          = TRUE
    )
    
    return(out_list)

  })
  
  output$print_surv_formula <- renderPrint({print(surv_fit()$form)})
  
  
  # Basic plots of KM once formula is done:
  
  # observeEvent(surv_fit(),{
  #   saveRDS(surv_fit(),"../debug/surv_fit.rds")
  # })
  
  
  output$ui_basic_km_plot <- renderPlot({
    req(!is.null(surv_fit()))
    surv_fit()$plot
  })
  
  
  
  # Run regression analysis
  
  flexsurv_regressions <- eventReactive(input$run_regressions,{
    
    # run regression analysis
    dists     <- c("exponential","weibull","llogis",      "lnorm",     "gompertz","gengamma")
    DistNames <- c("exponential","weibull","log_logistic","log_normal","gompertz","gen_gamma")
    names(dists) <- dists
    names(DistNames) <- DistNames
    
    return(
      lapply(dists, function(model){
        flexsurvreg(surv_fit()$form, data = surv_fit()$data, dist = dists[model])
      })
    )
  })
  
  output$print_flexsurv_regressions <- renderPrint({print(flexsurv_regressions())})
  output$print_flexsurv_regressions_coef <-
    renderPrint({
      print(lapply(flexsurv_regressions(), coefficients))
    })
  output$print_flexsurv_regressions_fit <-
    renderPrint({
      DistNames <- c("Exponential","Weibull","Log-logistic","Log-normal","Gompertz","Generalised gamma")
      fit_table <- do.call(rbind, lapply(flexsurv_regressions(), function(model) {
        a <- round(AIC(model),2)
        b <- round(BIC(model),2)
        c("AIC" = a, "BIC" = b)
      }))
      rn <- rownames(fit_table)
      rownames(fit_table) <- DistNames
      fit_table <- rownames_to_column(as.data.frame(fit_table),var = "distribution")
      rownames(fit_table) <- rn
      print(fit_table)
    })
  output$print_flexsurv_regressions_vcov <-
    renderPrint({
      print(lapply(flexsurv_regressions(), vcov))
    })
  
  
  
}
