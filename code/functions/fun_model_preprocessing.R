## ============================================================================================== ##
## Script:    Preprocess function for model fitting
## ============================================================================================== ##
## Authors:   Lukas J. Gunschera
## Date:      Tue Apr 23 17:32:50 2024
## ============================================================================================== ##
## @ raw_data:  raw task data to be modelled
## @ subjs:     character vector of subj IDs
## @ n_subj:    number of subjects
## @ t_subjs:   numeric vector with number of trails per subject
## ============================================================================================== ##

model_preprocessing <- function(raw_data,
                                retest = FALSE,
                                subjs,
                                n_subj,
                                t_subjs,
                                t_max) {

    # Initialize (model-specific) data arrays
    delay_sooner <- array( 0, c(n_subj, t_max))
    amount_sooner <- array( 0, c(n_subj, t_max))
    delay_later <- array( 0, c(n_subj, t_max))
    amount_later <- array( 0, c(n_subj, t_max))
    choice <- array(-1, c(n_subj, t_max))

    # Write from raw_data to the data arrays
    for (i in 1:n_subj) {
      subj <- subjs[i]
      t <- t_subjs[i]
      DT_subj <- raw_data[raw_data$subjID == subj,]

      delay_sooner[i, 1:t]   <- DT_subj$delay_sooner
      amount_sooner[i, 1:t]  <- DT_subj$amount_sooner
      delay_later[i, 1:t]  <- DT_subj$delay_later
      amount_later[i, 1:t] <- DT_subj$amount_later
      choice[i, 1:t]        <- DT_subj$choice
    }



    # Wrap into a list for Stan
    data_list <- list(
      N             = n_subj,
      T             = t_max,
      Tsubj         = t_subjs,
      delay_sooner   = delay_sooner,
      amount_sooner  = amount_sooner,
      delay_later  = delay_later,
      amount_later = amount_later,
      choice        = choice
    )

  # Returned data_list will directly be passed to Stan
  return(data_list)
}





