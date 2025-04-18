## ============================================================================================================================ ##
## Script:       FUNCTION: PREPARE MPLUS DATA
## ============================================================================================================================ ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2024-07-11
## ============================================================================================================================ ##
##
## ============================================================================================================================ ##

prepare_mplus_data <- function(df, filename, keepCols) {
  MplusAutomation::prepareMplusData(
    df = df,
    filename = here("data", "mplus", filename),
    inpfile = FALSE, overwrite = TRUE, imputed = FALSE,
    keepCols = keepCols
  )
}

# EXAMPLE USE ---------------------------------------------------------------------------------------------------------------

# tinvar_datasets <- list(
#  list(df = dat_mod_eatq, filename = "lcid_tinvar_mod_eatq.dat"),
#  list(df = dat_mod_bisbas, filename = "lcid_tinvar_mod_bisbas.dat"),
#  list(df = dat_mod_hscs, filename = "lcid_tinvar_mod_hscs.dat"),
#  list(df = dat_mod_sdq, filename = "lcid_tinvar_mod_sdq.dat")
# )

# lapply(tinvar_datasets, function(dataset) {
#  prepare_data(dataset$df, dataset$filename, c(tinvar_col, common_cols))
# })
