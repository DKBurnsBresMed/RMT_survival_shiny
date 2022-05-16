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
  
  output$ui_t_e_s_selector <- renderUI({
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
      ),
      fluidRow(
        width = 12,
        column(
          12,
          pickerInput(
            inputId = "strata",
            label = "Stratification factor",
            choices = raw_data_cols()[which(!raw_data_cols() %in% c(sel_time, sel_event))],
            selected = NULL,
            multiple = TRUE,
            width = "100%"
          )
        )
      )
    )
    
    
  })
  
  # Print out time, event, covariates, and strata
  output$print_time       <- renderPrint({print(input$time)})
  output$print_event      <- renderPrint({print(input$event)})
  output$print_covariates <- renderPrint({print(input$covariates)})
  output$print_factors    <- renderPrint({print(input$factors)})
  output$print_strata     <- renderPrint({print(input$strata)})
  
  
  # Survival fit object. This responds to the settings on strata, producing
  # a list with one object in it for one strata, 2 for 2 etc. All starts with
  # determining strata at the beginning, then uses the list elements from these
  # as an index for lapply of the rest of the process, so that the result is a
  # nested list of length N where N is the number of strata.
  
  observeEvent(input$generate_formula,priority = 9999,{
    saveRDS(raw_data(),"../debug/raw_data.rds")
    saveRDS(reactiveValuesToList(input),"../debug/input.rds")
  })
  
  surv_fit <- eventReactive(input$generate_formula, {
    
    req(!is.null(input$time))
    req(!is.null(input$event))
    
    
    # First, determine whether there is any stratification or not
    
    # logical - if there's nothing chosen for strata then its length will be 0.
    #           This then tells you whether the model is stratified or not.
    switch_strat <- length(input$strata) > 0
    
    # A list of strata is made IF and only if switch_strat is TRUE. Then unstratified
    # data is added to it at the end.
    
    # make an artifical identifier
    dat_list <- list()
    
    if (switch_strat == TRUE) {
      
      # This part is a bit more complicated. We need to cycle through the different
      # stratifiaction factor combinations, separating the dataset out into
      # reduced datasets, one for each subgroup matching that combination.
      # 
      # For example if we have 2 factors, which can both take values of 1 or 2,
      # we need 4 strata:
      # 
      # 1 1
      # 1 2
      # 2 1
      # 2 2
      #
      # to do this, we first make a table which pulls out the combinations
      # of unique values for those strata. Then, we cycle through the rows of
      # the table (one row for each unique combination), each time using the
      # function Reduce() to step through each stratification factor, filtering
      # down the data.frame to values matching the allowed value for that
      # strata. Reduce keeps the results and passes them to the next iteration
      # like a for loop, (as "prev" like in the below), and uses the index "it"
      # to choose the condition to apply next.
      #  
      
      # annoyingly the method has to be completely different for 1 stratification
      # factor, and multiple...
      
      if (length(input$strata) == 1) {
        
        unq_table <- unique(raw_data()[,input$strata])
        nam_list  <- paste0(input$strata,unq_table)
        names(nam_list) <- nam_list
        dat_list <- lapply(1:length(unq_table), function(combination) {
          raw_data()[which(raw_data()[,input$strata] == unq_table[combination]),]
        })
        names(dat_list) <- nam_list
        
        
      } else {
        # compile a table showing the cross-tabulation of the stratification factors
        unq_table <- unique(raw_data()[,input$strata])
        
        # figure out informative names for each strata by looping through and
        # making a unique id for each one
        nam_list <- unlist(
          lapply(1:nrow(unq_table), function(combination) {
            paste(
              paste0(colnames(unq_table),unq_table[combination,]),
              collapse = "_"
            )
          })
        )
        names(nam_list) <- nam_list
        
        # Cycle through the possible unique combinations of unique values and filter
        # down the data.frame in each case to produce a list of stratified datasets
        dat_list <- lapply(1:nrow(unq_table), function(combination) {
          unq_id <- as.list(unq_table[combination,])
          nam <- names(unq_id)
          return(
            Reduce(
              x = 1:length(unq_id),
              init = raw_data(),
              accumulate = FALSE,
              f = function(prev, it) {
                prev[which(prev[,nam[it]] == unq_id[[it]]),]
              }
            )
          )
        })
        names(dat_list) <- nam_list
        
      }
    }
    
    # add in the unstratified data, so that if there are strata we now have a list
    # of stratified datasets, plus a full dataset. 
    
    dat_list[["unstratified"]] <- raw_data()
    
    # grab the LHS and RHS variables:
    txt_inputs <- list(
      time  = input$time,
      event = input$event,
      cov   = input$covariates,
      fac   = input$factors
    )
    
    # labels for strata
    dat_labs <- names(dat_list)
    names(dat_labs) <- dat_labs
    
    # Now that we have a full list of all the datasets we want, we can cycle
    # through each dataset performing the survival analysis. This then
    # provides us with all of the possible survival analysis we could want.
    
    survest_list <- lapply(dat_labs, function(this_strata) {
      
      # Make an empty list, pull in the data for this strata, collect the
      # inputs for the regression analysis
      
      out_list <- list()
      out_list$df <- dat_list[[this_strata]]
      out_list$txt <- txt_inputs
      
      # generate the formula for use in flexsurv regression analysis, and 
      # perform survfit from the Surv package:
      
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
      
      
      # dataframe specifically for survminer.
      # TODO: Allow this to include factors and covariate adjustment!!!
      
      out_list$sm_df <- Func_MakeDatSurvFriendly(
        Data_required = out_list$df,
        time_column = out_list$txt$time,
        event_column = out_list$txt$event,
        t_multiplier = 1
      )
      
      # Generate survival estimates that can be used to generate survminer
      # Km plots, and then produce said plot
      # TODO: Allow this to put multiple strata on the same graph.
      
      out_list$est <- Func_GetSurvEst(Data_required = out_list$sm_df)
      out_list$plot <- Func_MakeKMPlotForUI(
        SurvEstimate  = out_list$est,
        Data_required = out_list$sm_df,
        censor_ticks  = TRUE,
        conf          = TRUE
      )
      
      # Now that we have already run everything for this strata, we may aswell
      # run flexsurv too!
      
      out_list$flexsurv <- lapply(dists, function(model){
        flexsurvreg(out_list$form, data = out_list$df, dist = dists[model])
      })
      
      out_list$vcov <- lapply(out_list$flexsurv, vcov)
      
      out_list$fit_table <- do.call(rbind, lapply(out_list$flexsurv, function(model) {
        a <- round(AIC(model),2)
        b <- round(BIC(model),2)
        c("AIC" = a, "BIC" = b)
      }))
      rn <- rownames(out_list$fit_table)
      rownames(out_list$fit_table) <- DistNames
      out_list$fit_table <- rownames_to_column(as.data.frame(out_list$fit_table),var = "distribution")
      rownames(out_list$fit_table) <- rn
      
      # get extrapolations
      # TODO: add an input for time unit of the analysis
      # TODO: add an input for time unit wanted in outputs
      # TODO: add server functions to perform time conversions
      # TODO: add an input for time horizon in time units defined by the user
      
      out_list$extraps <- get_curvefits(
        models = out_list$flexsurv,
        time = seq(from = 0, to = isolate(input$i_extrap_th), by = 1)
      )
      
      out_list$extrap_plots <- psm_plot(
        SurvEstimate = out_list$est,
        Data_required = out_list$sm_df,
        curvefits_data = out_list$extraps,
        xlim = isolate(input$i_extrap_th),
        break_by = round(isolate(input$i_extrap_th) / input$i_n_breaks, 0)
      )
      
      return(out_list)
      
    })
    
    return(survest_list)
  })
  
  output$print_surv_formula <- renderPrint({print(lapply(surv_fit(),function(strata) strata$form))})
  
  
  # Basic plots of KM once formula is done:
  
  # observeEvent(surv_fit(),{
  #   saveRDS(surv_fit(),"../debug/surv_fit.rds")
  # })
  
  n_plots <- eventReactive({
    surv_fit()
  },{
    # figure out how many plots are required (the number of strata)
    length(surv_fit())
  })
  
  observe({
    lapply(1:n_plots(), function(strata) {
      output[[paste0("ui_basic_km_plot",strata)]] <- renderPlot({
        surv_fit()[[strata]]$plot
      })
      output[[paste0("ui_extrap_plot",strata)]] <- renderPlot({
        surv_fit()[[strata]]$extrap_plots
      })
      output[[paste0("ui_aicbic",strata)]] <- renderDataTable({
        datatable(
          data = surv_fit()[[strata]]$fit_table,
          options = Config_UI_Res_BDTabopts,
          caption = paste0("Fit statistics for ", names(surv_fit())[strata])
        )
      })
      lapply(dists,function(dist) {
        output[[paste0("ui_vcov_",dist,strata)]] <- renderDataTable({
          datatable(
            data = surv_fit()[[strata]]$vcov[[dist]],
            options = Config_UI_Res_BDTabopts,
            caption = paste0("Variance covariance for ", names(surv_fit())[strata], ": ", dist)
          )
        })
      })
      
    })
  }, priority = 1000)

  output$ui_basic_km_plots <- renderUI({
    
    # list of plots to make:
    n_plots <- length(surv_fit())
    
    plot_list <- lapply(1:n_plots, function(this_plot) {
      tabPanel(
        title = names(surv_fit())[this_plot],
        value = paste0("strata",this_plot),
        icon = icon("eye"),
        fluidRow(
          width = 12,
          column(
            6,
            plotOutput(paste0("ui_basic_km_plot",this_plot))
          ),
          column(
            6,
            plotOutput(paste0("ui_extrap_plot",this_plot))
          )
        ),
        fluidRow(
          width = 12,
          column(
            6,
            lapply(dists, function(dist) {
              tagList(
                br(),
                dataTableOutput(paste0("ui_vcov_",dist,this_plot)),
                hr()
              )
            })
          ),
          column(
            6,
            dataTableOutput(paste0("ui_aicbic",this_plot))
          )
        )
      )
    })
    
    # put all of them together into a taglist:
    
    do.call(tabBox, c(
      plot_list,
      list(
        id = "tab_plotoutput",
        width = "100%",
        selected = isolate(input$tab_plotoutput)
      )
    ))
    
    
  })
  
  
  
  
  
  
  # observe({print(input$tab_survplot_output)})
  
  # tab_survplot <- reactive({
  #   t <- isolate(input$tab_survplot_output)
  #   if (is.null(t)) {
  #     return("strata1")
  #   } else {
  #     return(t)
  #   }
  # })
  
  
  
  # Run regression analysis
  
  output$print_flexsurv_regressions <-
    renderPrint({
      print(lapply(surv_fit(), function(strata){
        strata$flexsurv 
      }))
    })
  output$print_flexsurv_regressions_coef <-
    renderPrint({
      print(lapply(surv_fit(), function(strata){
        lapply(strata$flexsurv, coefficients)
      }))
    })
  output$print_flexsurv_regressions_fit <-
    renderPrint({
      print(lapply(surv_fit(), function(strata){
        strata$fit_table 
      }))
    })
  output$print_flexsurv_regressions_vcov <-
    renderPrint({
      print(lapply(surv_fit(), function(strata){
        strata$vcov
      }))
    })
  
  
  
}
