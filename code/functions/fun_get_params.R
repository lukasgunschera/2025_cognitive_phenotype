## ======================================================================================================================= ##
## Script:    FUNCTION: GET PARAMETER ESTIMATES
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Tue Apr 23 17:33:44 2024
## ======================================================================================================================= ##
## @ subj_id:     vector of subject ids
## @ model_fit:   model fit object
## @ n_subj:      sample size
## @ n_params:    number of parameters
## @ param_names: character vector with names of parameters
## ======================================================================================================================= ##

## Extract individual and group-level parameter estimates -------------------------------------------------------------------
get_params <- function(subj_id,
                       model_fit,
                       n_subj,
                       n_params,
                       param_names) {
  # Group-level parameters
  group_params <- tibble(
    "parameter" = paste("mu", param_names, sep = "_"),
    "estimate" = model_fit$draws(
      variables = paste("mu", param_names, sep = "_"),
      format = "df", inc_warmup = FALSE
    ) %>%
      colMeans() %>% unname() %>% .[1:n_params]
  )

  # Individual-level parameters
  individual_params <- tibble(
    "subj_id" = rep(subj_id, n_params),
    "parameter" = rep(param_names, each = n_subj),
    "estimate" = model_fit$draws(
      variables = param_names,
      format = "df", inc_warmup = FALSE
    ) %>%
      colMeans() %>% unname() %>% .[1:(n_subj * n_params)],
    "hdi_lower" = model_fit$draws(
      variables = param_names,
      format = "df", inc_warmup = FALSE
    ) %>%
      map(hBayesDM::HDIofMCMC) %>%
      as_tibble() %>% .[1, 1:(n_subj * n_params)] %>% as_vector() %>% unname(),
    "hdi_upper" = model_fit$draws(
      variables = param_names,
      format = "df", inc_warmup = FALSE
    ) %>%
      map(hBayesDM::HDIofMCMC) %>%
      as_tibble() %>% .[2, 1:(n_subj * n_params)] %>% as_vector() %>% unname()
  )


  return(list(
    "group_params" = group_params,
    "individual_params" = individual_params
  ))
}
