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

