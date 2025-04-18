## ======================================================================================================================= ##
## Script:    FUNCTION: MISCELLANEOUS HELPER FUNCTIONS
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Thu Mar 27 14:30:27 2025
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##


### Helper functions used in subsequent functions and code


# SUBSET THREE PARTICIPANTS -------------------------------------------------------------------------------------------------
subset_x_participants <- function(data, fit_cluster, participants) {
  if (!fit_cluster) {
    # Assuming 'subjID' is the column name for individual IDs
    unique_ids <- unique(data$subjID)
    if (length(unique_ids) > participants) {
      ids <- unique_ids[1:participants]
      data <- data[data$subjID %in% ids, ]
    }
  }
  return(data)
}

# EXAMPLE USE
# subset_three_participants(dd_data[[1]], FIT_CLUSTER)
# selects the first three unique subjID identifiers and extract an object

## Add sample function ------------------------------------------------------------------------------------------------------
add_sample <- function(x) {
  return(c(
    y = max(x) + .025,
    label = length(x)
  ))
}

## Population standard deviation --------------------------------------------------------------------------------------------
# @ x: A numerical vector of values

sdP <- function(x) {
  sqrt(mean(x^2) - (mean(x))^2)
}

## Reverse code questionnaire items -----------------------------------------------------------------------------------------
# @ x: Column or column vector
reverse_code <- function(x) {
  f <- factor(x)
  y <- rev(levels(f))[f]
  class(y) <- class(x)
  return(y)
}

## Standard error -----------------------------------------------------------------------------------------------------------
# @ x: A numerical vector of values

se <- function(x) sqrt(var(x) / length(x))

## Mode ---------------------------------------------------------------------------------------------------------------------
# @ x: A numerical vector of values

getmode <- function(x) {
  uniqX <- unique(x)
  uniqX[which.max(tabulate(match(x, uniqX)))]
}

## Participant wise maximum trial calculation -------------------------------------------------------------------------------
# alterantive to stats::aggregate, due to different ordering algorithms

max_trial_per_subjID <- function(task_data) {
  unique_subjIDs <- unique(task_data$subjID)
  max_trials <- numeric(length(unique_subjIDs))

  for (i in seq_along(unique_subjIDs)) {
    subjID <- unique_subjIDs[i]
    max_trials[i] <- max(task_data$trial[task_data$subjID == subjID])
  }

  max_trials
}

## Task data standardization ------------------------------------------------------------------------------------------------
# @ x: A numerical vector of values
# @ ref: A single numerical values to be used as the reference
# @ levels: Possible values the data to be standardized can take

standardization <- function(x, ref = 0, levels = 1:4) {
  (x - ref) / sdP(levels)
}


## Scale variables to 0-1 ---------------------------------------------------------------------------------------------------
# @ x: Variable vector

rescale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}


## Normalize negatively skewed data -----------------------------------------------------------------------------------------
# @ x: Variable vector

norm_neg_skew <- function(x) {
  (1 - sqrt(1 - x))
}

## Remove High NA Columns (default = 90%) -----------------------------------------------------------------------------------
remove_high_na_columns <- function(df, threshold = 0.90) {
  # Calculate the proportion of NA values in each column
  na_proportion <- sapply(df, function(x) mean(is.na(x)))

  # Identify columns to keep and to remove
  cols_to_keep <- names(na_proportion[na_proportion <= threshold])
  cols_to_remove <- names(na_proportion[na_proportion > threshold])

  # Subset the dataframe to keep only the columns that meet the threshold criterion
  df_clean <- df %>% select(all_of(cols_to_keep))

  # Output the columns that were removed
  cat("Columns removed due to high NA proportion (> threshold% NA):\n")
  print(cols_to_remove)

  return(df_clean)
}

# Get Posterior Predictive Correlations -------------------------------------------------------------------------------------
ppc_correlate <- function(data, dataset_name) {
  data %>%
    group_by() %>%
    summarise(
      correlation = cor(mean_pred_choice, mean_real_choice),
      hdi_lower = mean(hdi_lower_pred_choice),
      hdi_upper = mean(hdi_upper_pred_choice)
    ) %>%
    mutate(dataset = dataset_name)
}
