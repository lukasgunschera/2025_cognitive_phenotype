## ============================================================================================================================ ##
## Script:    Posterior predictive check function
## ============================================================================================================================ ##
## Authors:   Lukas J. Gunschera
## Date:      Mon Jul  8 16:05:36 2024
## ============================================================================================================================ ##
## @ model_fit:  cmdstanr model fitting output
## @ n_iter:     number of iterations
## @ n_subj:     number of subjects
## @ n_trials:   number of trials
## @ real_dat:   real data (observed)
## ============================================================================================================================ ##

# Define the function to apply to each element
add_hdis <- function(element) {
  element %>%
    group_by(subj, real_delay_later) %>%
    summarise(
      mean_real_choice = mean(real_choice),
      mean_pred_choice = mean(pred_choice),
      hdi_lower_pred_choice = HDIofMCMC(pred_choice) %>% first(.),
      hdi_upper_pred_choice = HDIofMCMC(pred_choice) %>% last(.)
    )
}

posterior_predictions <- function(model_fit,
                                  n_subj,
                                  n_trials,
                                  real_dat) {

  pred_var_names <- as.vector(
    sapply(1:n_subj, function(i)
      sapply(1:n_trials, function(j) paste0("y_pred[", i, ",", j, "]"))
    )
  )

  # Extract posterior draws for all specified variables
  draws <- model_fit$draws(variables = pred_var_names, format = "draws_matrix")

  # Create a list to store results for each subject
  ppc_subj_list <- lapply(1:n_subj, function(id) {

    s_trials <- real_dat$Tsubj[id]

    # Extract relevant columns for the current subject
    subj_draws <- draws[, grepl(paste0("y_pred\\[", id, ","), colnames(draws))]

    # Calculate the mean and HDI for each trial
    pred_means <- colMeans(subj_draws)
    pred_hdi <- apply(subj_draws, 2, HDIofMCMC)

    tibble(subj = id,
           real_delay_sooner = real_dat$delay_sooner[id, 1:s_trials],
           real_amount_sooner = real_dat$amount_sooner[id, 1:s_trials],
           real_delay_later = real_dat$delay_later[id, 1:s_trials],
           real_amount_later = real_dat$amount_later[id, 1:s_trials],
           real_choice = real_dat$choice[id, 1:s_trials],
           pred_choice = pred_means[1:s_trials],
           pred_hdi_upper = pred_hdi[1, 1:s_trials],
           pred_hdi_lower = pred_hdi[2, 1:s_trials])
  })

  # Add HDIs for estimations
  ppc_subj_list <- lapply(ppc_subj_list, add_hdis)


  # Combine all subject results into a single data frame
  ppc <- bind_rows(ppc_subj_list)

  return(ppc)
}
