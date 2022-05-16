#'function: SurvregFormulaGen
#'Purpose: generate a formula object 
#'Author: Darren Burns
#'Inst.: BresMed
#'Date: 28/08/2019
#'Ver.: 0.0.1
SurvregFormulaGen <- function(t,e,covs,factors,nam_t,nam_e,nam_covs,nam_factors = NULL,treat_factor = FALSE,nam_Treat,DEBUG=FALSE) {
  # make sure time and event data is numeric
  t <- as.numeric(t)
  e <- as.numeric(e)
  
  # simple formula if there are no covariates
  if (all(is.null(nam_covs), is.null(nam_factors))) {
    survreg_formula <- "Surv(t, e) ~ 1"
  } else {
    
    # more complicated formula if there are covariates/factors at play
    #start from the basic:
    survreg_formula <- "Surv(t, e) ~ "
    
    # add in the rest using a logical framework for the model specification
    
    if (all(is.null(nam_covs), !is.null(nam_factors))) {
      # no covariates but categorical (factor) variables included
      # Add in these categoricals one at a time
      for (i in 1:length(nam_factors)) {
        if (i == 1) {
          #The first one should take the formula and add in the first covariate 
          #    WITHOUT a + in front of it
          survreg_formula <- paste0(survreg_formula, "factor(",nam_factors[1],")")
        } else {
          #all the additional covariates will have a + separating it from the others
          survreg_formula <- paste0(survreg_formula, " + factor(", nam_factors[i],")")
        }
      }
    } else if(all(!is.null(nam_covs), is.null(nam_factors))) {
      #no stratifications but continuous variables included
      # Add in the covariates one at a time
      for (i in 1:length(nam_covs)) {
        if (i == 1) {
          #The first one should take the formula and add in the first covariate 
          #    WITHOUT a + in front of it
          survreg_formula <- paste0(survreg_formula, nam_covs[1])
        } else {
          #all the additional covariates will have a + separating it from the others
          survreg_formula <- paste0(survreg_formula, " + ", nam_covs[i])
        }
      }
    } else {
      #both stratifications and covariates
      for (i in 1:length(nam_covs)) {
        if (i == 1) {
          #The first one should take the formula and add in the first covariate 
          #    WITHOUT a + in front of it
          survreg_formula <- paste0(survreg_formula, nam_covs[1])
        } else {
          #all the additional covariates will have a + separating it from the others
          survreg_formula <- paste0(survreg_formula, " + ", nam_covs[i])
        }
        
      }
      # now add in the stratification factors one at a time
      for (i in 1:length(nam_factors)) {
        survreg_formula <- paste0(survreg_formula, " + factor(", nam_factors[i],")")
      }
    }
  }
  
  # Final step: If ARM included as a covariate, include it here as a factor.
  if (treat_factor) {
    #if treatment is covariate, then add it to specification as a catagorical variable
    survreg_formula <- paste0(survreg_formula, " + factor(",nam_Treat,")")
  }
  
  # Debug output: print formula to console
  if (DEBUG) {
    print(survreg_formula)
  }
  
  # Now define the final string as a formula object to be used in flexsurv estimation
  survreg_formula <- as.formula(survreg_formula)
  
  # saveRDS(survreg_formula, "../debug/survreg_formula.rds")
  
  # This formula is then passed into the regression analysis
  return(survreg_formula)
  
}

# # Testing example:
# t <- ovarian[, "futime"] %>% as.numeric()
# e <- ovarian[, "fustat"] %>% as.numeric()
# covs <- ovarian[, "age"] %>% as.numeric()
# Factors <- ovarian[, "rx"] %>% as.factor()
# 
# nam_t <- "futime"
# nam_e <- "fustat"
# nam_covs <- c("age")
# nam_factors <- c("rx","resid.ds")
# SurvregFormulaGen(
#   t = t,
#   e = e,
#   covs = covs,
#   factors = factors,
#   nam_t = nam_t,
#   nam_e = nam_e,
#   nam_covs = nam_covs,
#   nam_factors = nam_factors,
#   DEBUG = TRUE
# )



# Make the KM plot data ---------------------------------------------------

MakeKMPlotData <- function(DATA, timevar, eventvar) {
  #make KM  testing ground
  #load example data and do survfit
  x <- DATA
  y <- Surv(x[,timevar], x[,eventvar])
  z <- survfit(y ~ 1)
  
  #make adjustments to data required for graph (i.e. double both datasets)
  t   <- rep(z$time,2)
  s_t <- rep(z$surv,2)
  
  #sort parameters as required (by time ascending, by survival descending), adding extra data points
  t   <- append(0,t[order(t)])
  s_t <- append(1,append(1,s_t[order(s_t,decreasing = TRUE)]))[1:length(t)]
  
  #put into a dataframe and return as output of function
  df <- data.frame(t = t, s_t = s_t)
  return(df)
}



MakeSMKMPlot_single <- function(DATA, fit) {
  survminer::ggsurvplot(
    fit = fit,
    data = DATA,
    risk.table = TRUE
  )
}


MakeSMKMPlot_strat <- function(DATA, formula) {
  z <- survfit(formula)
  survminer::ggsurvplot(
    fit = z,
    data = DATA,
    risk.table = TRUE
  )
}


# EXAMPLE
# o         <- ovarian
# txt_time  <- "futime"
# txt_event <- "fustat"
# est_list <- list(
#   data     = Func_MakeDatSurvFriendly(
#     Data_required = o,
#     time_column = txt_time,
#     event_column = txt_event,
#     t_multiplier = 1
#   )
# )
# est_list$est <- Func_GetSurvEst(Data_required = est_list$data)
# est_list$plot <- Func_MakeKMPlotForUI(
#   SurvEstimate  = est_list$est,
#   Data_required = est_list$data,
#   censor_ticks  = TRUE,
#   conf          = TRUE
# )





Func_MakeDatSurvFriendly <- function(Data_required, time_column, event_column, t_multiplier = 1) {
  dat <- Data_required[,c(time_column,event_column)]
  colnames(dat) <- c("time", "event")
  dat[,"time"] <- dat[,"time"] * t_multiplier
  return(dat)
}

Func_GetSurvEst <- function(Data_required) {
  survfit(Surv(time, event) ~ 1, data = Data_required)
}

Func_KM_GenEstList <- function(t_unit, CL, Data, t_col, e_col, t_trans = "cycles") {
  
  # decide if the time column is to be transformed into model cycles or into years
  if (t_trans == "cycles") {
    t_multiplier <- Func_KM_t_2_CL(t_unit = t_unit, CL = CL)
  } else {
    t_multiplier <- Func_KM_t_2_yr(t_unit = t_unit, CL = CL)
  }
  
  # get the data ready for survminer
  Dat <- Func_MakeDatSurvFriendly(
    Data_required = Data,
    time_column   = t_col,
    event_column  = e_col,
    t_multiplier  = t_multiplier
  )
  
  # generate survival estimates
  Est <- Func_GetSurvEst(Data_required = Dat)
  
  # return a list of the data and the estimate
  return(list(
    Data     = Dat,
    Estimate = Est
  ))
}

Func_try_survest <- function(r_basic, r_pld, this_arm, this_endpoint, this_time_unit) {
  
  # try to run this estimation, catching the error and returning NA if so:
  
  rawdat_nam <- paste0(this_arm,"_",this_endpoint,"Data")
  
  Est <- try(
    Func_KM_GenEstList(
      t_unit  = r_pld$time_units[[this_arm]][[this_endpoint]],
      CL      = r_basic$CycleLength,
      Data    = r_pld$raw_data[[rawdat_nam]],
      t_col   = r_pld$cols[[this_arm]][[this_endpoint]]$t,
      e_col   = r_pld$cols[[this_arm]][[this_endpoint]]$e,
      t_trans = this_time_unit
    ),
    silent = TRUE
  )
  
  # when using try the model will ignore errors. make sure you code in a custom message.
  # If you don't do this, you can end up not being able to find an error taking place!
  
  if(class(Est) == "try-error") {
    warning(paste0(
      "Failed: ",
      this_endpoint,
      " for ",
      this_arm
    ))
    
    # only print out the actual error message if it's not "supposed" to error out
    # (i.e. only when there's actually data to perform KM analysis on)
    if (!is.logical(r_pld$raw_data[[rawdat_nam]])) warning(Est)
    
    Est <- NA
  }
  
  return(Est)
}

Func_produce_KMEsts <- function(Basic, PLD) {
  
  # basically, cycle through the arms and the endpoints
  # trying to do the conversion to survminer format all in one.
  # This contains automatic error handling too, so it doesn't
  # even matter if it fails to run things. Produces both
  # in model cycles and in model years.
  
  arms <- c(Int = "Int", Comp = "Comp")
  endp <- c(PFSRFS = "PFSRFS", OS = "OS", TOT = "TOT")
  
  lapply(arms, function(this_arm) {
    cycles <- lapply(endp, function(this_endpoint){Func_try_survest(Basic, PLD, this_arm, this_endpoint, "cycles")})
    years <- lapply(endp, function(this_endpoint){Func_try_survest(Basic, PLD, this_arm, this_endpoint, "years")})
    list(cycles = cycles, years = years)
  })
  
}


# Func_MakeKMPlotForUI
Func_MakeKMPlotForUI <- function(SurvEstimate, Data_required, censor_ticks, conf) {
  ggsurvplot(
    fit = SurvEstimate,
    data = Data_required,
    risk.table = T,
    palette = "black",
    surv.median.line = "hv",
    legend = "none",
    censor = censor_ticks,
    conf.int = conf
  )
}

# Func_Make_km_summary
Func_Make_km_summary <- function(SurvEstimate) {
  DF =  summary(SurvEstimate)$table %>%
    t() %>%
    as.data.frame()  %>%
    dplyr::mutate(
      Censors = records - events,
      Median_CI = paste0(
        round(median, 2),
        " (",
        round(`0.95LCL`, 2),
        ", ",
        round(`0.95UCL`, 2),
        ")"
      )
    ) %>%
    dplyr::select(Subjects = records, Events = events, Censors, Median_CI)
}


# Func_MakeKMLCHPForUI
Func_MakeKMLCHPForUI <- function(SurvEstimate, Data_required, censor_ticks, conf) {
  ggsurvplot(
    fit = SurvEstimate,
    data = Data_required,
    fun = 'cloglog',
    palette = "black",
    legend = "none",
    censor = censor_ticks,
    conf.int = conf
  )
}

Func_KM_ProcPlot <- function(PlotDatList, Cens, conf) {
  
  # get relevant data
  Est <- PlotDatList$Estimate
  Dat <- PlotDatList$Data
  
  # make KM plot
  KM_plot <- Func_MakeKMPlotForUI(
    SurvEstimate  = Est,
    Data_required = Dat,
    censor_ticks  = Cens,
    conf          = conf
  )
  KM_plot <- KM_plot + ggplot2::labs(x = paste0("Years"))
  
  # summary table
  Table <- Func_Make_km_summary(SurvEstimate = Est)
  
  # log-cumulative hazard plot
  LCHP <- Func_MakeKMLCHPForUI(
    SurvEstimate = Est,
    Data_required = Dat,
    censor_ticks = Cens,
    conf = conf
  )
  
  # output list
  return(list(
    KM = KM_plot,
    summary = Table,
    LCHP = LCHP
  ))
}







