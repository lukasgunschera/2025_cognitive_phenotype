## ============================================================================================================================ ##
## Script:    Function to load modeling results
## ============================================================================================================================ ##
## Authors:   Lukas J. Gunschera
## Date:      Thu May 16 11:27:01 2024
## ============================================================================================================================ ##
##
## ============================================================================================================================ ##

load_model_results <- function(data_location, posterior_predictive = FALSE, timeinvariant = FALSE, folder = "dd_hyperbolic") {
  ## Set Data Path depending on whether it should be loadded from project repository or cluster ------------------------------- ##
  if (data_location == "local") {
    ## Local Path ------------------------------------------------------------------------------------------------------------- ##
    dat_path <- here::here("output", "lcid", "dd_delaydiscount", "modelfit", folder)
  } else {
    ## Cluster path ----------------------------------------------------------------------------------------------------------- ##
    dat_path <- paste("Z:/projects/2024_gunschera_lcid/output/modelfit/", folder, sep = "")
  }

  if(timeinvariant) {

    ## Data collapsed across all waves ---------------------------------------------------------------------------------------- ##
    dd_hyperbo_tinvar_check <- readRDS(paste0(dat_path, "/tinvar/dd_hyperbo_check_tinvar.RDS"))
    dd_hyperbo_tinvar_loo <- readRDS(paste0(dat_path, "/tinvar/dd_hyperbo_loo_tinvar.RDS"))
    dd_hyperbo_tinvar_params <- readRDS(paste0(dat_path, "/tinvar/dd_hyperbo_parameters_tinvar.RDS"))
    if(posterior_predictive){dd_hyperbo_tinvar_ppc <- readRDS(paste0(dat_path, "/tinvar/dd_hyperbo_ppc_tinvar.RDS"))}

    ## Return Results --------------------------------------------------------------------------------------------------------- ##
    return(list(
      dd_hyperbo_tinvar_check = dd_hyperbo_tinvar_check,
      dd_hyperbo_tinvar_loo = dd_hyperbo_tinvar_loo,
      dd_hyperbo_tinvar_params = dd_hyperbo_tinvar_params,
      dd_hyperbo_tinvar_ppc = if (posterior_predictive) dd_hyperbo_tinvar_ppc else NULL
    ))
  } else {

  ## Wave 02 ---------------------------------------------------------------------------------------------------------------- ##
  dd_hyperbo_check_02 <- readRDS(paste0(dat_path, "/wave2/dd_hyperbo_check_02.RDS"))
  dd_hyperbo_loo_02 <- readRDS(paste0(dat_path, "/wave2/dd_hyperbo_loo_02.RDS"))
  dd_hyperbo_params_02 <- readRDS(paste0(dat_path, "/wave2/dd_hyperbo_parameters_02.RDS"))
  if(posterior_predictive){dd_hyperbo_ppc_02 <- readRDS(paste0(dat_path, "/wave2/dd_hyperbo_ppc_02.RDS"))}

  ## Wave 03 ---------------------------------------------------------------------------------------------------------------- ##
  dd_hyperbo_check_03  <- readRDS(paste0(dat_path, "/wave3/dd_hyperbo_check_03.RDS"))
  dd_hyperbo_loo_03    <- readRDS(paste0(dat_path, "/wave3/dd_hyperbo_loo_03.RDS"))
  dd_hyperbo_params_03 <- readRDS(paste0(dat_path, "/wave3/dd_hyperbo_parameters_03.RDS"))
  if(posterior_predictive){dd_hyperbo_ppc_03 <- readRDS(paste0(dat_path, "/wave3/dd_hyperbo_ppc_03.RDS"))}

  ## Wave 04 ---------------------------------------------------------------------------------------------------------------- ##
  dd_hyperbo_check_04  <- readRDS(paste0(dat_path, "/wave4/dd_hyperbo_check_04.RDS"))
  dd_hyperbo_loo_04    <- readRDS(paste0(dat_path, "/wave4/dd_hyperbo_loo_04.RDS"))
  dd_hyperbo_params_04 <- readRDS(paste0(dat_path, "/wave4/dd_hyperbo_parameters_04.RDS"))
  if(posterior_predictive){dd_hyperbo_ppc_04 <- readRDS(paste0(dat_path, "/wave4/dd_hyperbo_ppc_04.RDS"))}

  ## Wave 05 ---------------------------------------------------------------------------------------------------------------- ##
  dd_hyperbo_check_05  <- readRDS(paste0(dat_path, "/wave5/dd_hyperbo_check_05.RDS"))
  dd_hyperbo_loo_05    <- readRDS(paste0(dat_path, "/wave5/dd_hyperbo_loo_05.RDS"))
  dd_hyperbo_params_05 <- readRDS(paste0(dat_path, "/wave5/dd_hyperbo_parameters_05.RDS"))
  if(posterior_predictive){dd_hyperbo_ppc_05 <- readRDS(paste0(dat_path, "/wave5/dd_hyperbo_ppc_05.RDS"))}

  ## Wave 06 ---------------------------------------------------------------------------------------------------------------- ##
  dd_hyperbo_check_06  <- readRDS(paste0(dat_path, "/wave6/dd_hyperbo_check_06.RDS"))
  dd_hyperbo_loo_06    <- readRDS(paste0(dat_path, "/wave6/dd_hyperbo_loo_06.RDS"))
  dd_hyperbo_params_06 <- readRDS(paste0(dat_path, "/wave6/dd_hyperbo_parameters_06.RDS"))
  if(posterior_predictive){dd_hyperbo_ppc_06 <- readRDS(paste0(dat_path, "/wave6/dd_hyperbo_ppc_06.RDS"))}

  ##============================================================================================================================= ##
  ## Return loaded results
  ## ============================================================================================================================ ##
  return(list(
    dd_hyperbo_check_02 = dd_hyperbo_check_02,
    dd_hyperbo_loo_02 = dd_hyperbo_loo_02,
    dd_hyperbo_params_02 = dd_hyperbo_params_02,
    dd_hyperbo_ppc_02 = if (posterior_predictive) dd_hyperbo_ppc_02 else NULL,

    dd_hyperbo_check_03 = dd_hyperbo_check_03,
    dd_hyperbo_loo_03 = dd_hyperbo_loo_03,
    dd_hyperbo_params_03 = dd_hyperbo_params_03,
    dd_hyperbo_ppc_03 = if (posterior_predictive) dd_hyperbo_ppc_03 else NULL,

    dd_hyperbo_check_04 = dd_hyperbo_check_04,
    dd_hyperbo_loo_04 = dd_hyperbo_loo_04,
    dd_hyperbo_params_04 = dd_hyperbo_params_04,
    dd_hyperbo_ppc_04 = if (posterior_predictive) dd_hyperbo_ppc_04 else NULL,

    dd_hyperbo_check_05 = dd_hyperbo_check_05,
    dd_hyperbo_loo_05 = dd_hyperbo_loo_05,
    dd_hyperbo_params_05 = dd_hyperbo_params_05,
    dd_hyperbo_ppc_05 = if (posterior_predictive) dd_hyperbo_ppc_05 else NULL,

    dd_hyperbo_check_06 = dd_hyperbo_check_06,
    dd_hyperbo_loo_06 = dd_hyperbo_loo_06,
    dd_hyperbo_params_06 = dd_hyperbo_params_06,
    dd_hyperbo_ppc_06 = if (posterior_predictive) dd_hyperbo_ppc_06 else NULL
  ))

  }
}
