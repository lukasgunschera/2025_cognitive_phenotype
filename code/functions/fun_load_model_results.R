## ======================================================================================================================= ##
## Script:    FUNCTION: LOAD MODELING RESULTS
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Thu May 16 11:27:01 2024
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

load_model_results <- function(data_location, posterior_predictive = FALSE, folder = "modelfit", load_local = TRUE) {
  dat_path <- here::here("output", folder)

  if (load_local) {
    #### Wave 2 -------------------------------------------------------------------------------------------------------------
    dd_hyperbo_check_02 <- readRDS(paste0(dat_path, "/dd_hyperbo_check_02.RDS"))
    dd_hyperbo_loo_02 <- readRDS(paste0(dat_path, "/dd_hyperbo_loo_02.RDS"))
    dd_hyperbo_params_02 <- readRDS(paste0(dat_path, "/dd_hyperbo_parameters_02.RDS"))
    if (posterior_predictive) {
      dd_hyperbo_ppc_02 <- readRDS(paste0(dat_path, "/dd_hyperbo_ppc_02.RDS"))
    }

    #### Wave 3 -------------------------------------------------------------------------------------------------------------
    dd_hyperbo_check_03 <- readRDS(paste0(dat_path, "/dd_hyperbo_check_03.RDS"))
    dd_hyperbo_loo_03 <- readRDS(paste0(dat_path, "/dd_hyperbo_loo_03.RDS"))
    dd_hyperbo_params_03 <- readRDS(paste0(dat_path, "/dd_hyperbo_parameters_03.RDS"))
    if (posterior_predictive) {
      dd_hyperbo_ppc_03 <- readRDS(paste0(dat_path, "/dd_hyperbo_ppc_03.RDS"))
    }

    #### Wave 4 -------------------------------------------------------------------------------------------------------------
    dd_hyperbo_check_04 <- readRDS(paste0(dat_path, "/dd_hyperbo_check_04.RDS"))
    dd_hyperbo_loo_04 <- readRDS(paste0(dat_path, "/dd_hyperbo_loo_04.RDS"))
    dd_hyperbo_params_04 <- readRDS(paste0(dat_path, "/dd_hyperbo_parameters_04.RDS"))
    if (posterior_predictive) {
      dd_hyperbo_ppc_04 <- readRDS(paste0(dat_path, "/dd_hyperbo_ppc_04.RDS"))
    }

    #### Wave 5 -------------------------------------------------------------------------------------------------------------
    dd_hyperbo_check_05 <- readRDS(paste0(dat_path, "/dd_hyperbo_check_05.RDS"))
    dd_hyperbo_loo_05 <- readRDS(paste0(dat_path, "/dd_hyperbo_loo_05.RDS"))
    dd_hyperbo_params_05 <- readRDS(paste0(dat_path, "/dd_hyperbo_parameters_05.RDS"))
    if (posterior_predictive) {
      dd_hyperbo_ppc_05 <- readRDS(paste0(dat_path, "/dd_hyperbo_ppc_05.RDS"))
    }

    #### Wave 6 -------------------------------------------------------------------------------------------------------------
    dd_hyperbo_check_06 <- readRDS(paste0(dat_path, "/dd_hyperbo_check_06.RDS"))
    dd_hyperbo_loo_06 <- readRDS(paste0(dat_path, "/dd_hyperbo_loo_06.RDS"))
    dd_hyperbo_params_06 <- readRDS(paste0(dat_path, "/dd_hyperbo_parameters_06.RDS"))
    if (posterior_predictive) {
      dd_hyperbo_ppc_06 <- readRDS(paste0(dat_path, "/dd_hyperbo_ppc_06.RDS"))
    }

    ### Return Results ------------------------------------------------------------------------------------------------------
    return(
      list(
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
      )
    )
  } else {
    print("Please set load_local to TRUE to load the data locally.")
  }
}
