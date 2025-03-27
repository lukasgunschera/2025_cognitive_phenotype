## ============================================================================================================================ ##
## Script:       PREPROCESS DELAY DISCOUNTING DATA
## ============================================================================================================================ ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2024-07-11
## ============================================================================================================================ ##

## SETUP ====================================================================================================================

library(renv)
renv::restore()

set.seed(777) # set seed for random processes

# load required packages
library(here)
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(ggpubr)
library(stringr)
library(ggplot2)
library(ggpmisc)
library(viridis)
library(magrittr)
library(viridisLite)

# set directory
here::i_am("renv.lock") # set directory

# load custom functions
source(here::here("code", "functions", "fun_plots.R"))
source(here::here("code", "functions", "fun_helper.R"))
source(here::here("code", "functions", "fun_get_params.R"))
source(here::here("code", "functions", "fun_convergence_check.R"))
source(here::here("code", "functions", "fun_model_preprocessing.R"))
source(here::here("code", "functions", "fun_posterior_predictions.R"))

## PREPROCESS FUNCTION ======================================================================================================

dd_preprocess <- function(file_paths) {

  output_list <- list() # initiate empty object for output

  # loop over file paths and process each individually
  for (file_path in file_paths) {

    # load current file_path iteration and process
    dd_file <- read_csv(here("data", "raw", file_path))

    # select relevant columns, (data format was distinct in wave 6)
    if (file_path == "ses_w06_delaydiscounting.csv") {
      dd_file %<>%
        dplyr::select("Tijdstempel", "SU PPN", "Trial record")     # SU PPN = Subject ID in Wave 2,3,4,5
    } else {
      dd_file %<>%
        dplyr::select("Tijdstempel", "Subject ID", "Trial record") # Subject ID = SU PPN in Wave 6
    }

    # Rename columns
    colnames(dd_file) <- c("time", "subjID", "results")

    # Separate results column into individual trials
    dd_temp <- dd_file %>%
      separate_rows(results, sep = "\\|", convert = TRUE) %>%
      dplyr::filter(!is.na(results)) %>%               # remove rows with empty results
      dplyr::filter(str_length(subjID) == 8) %>%       # remove rows with invalid participant IDs
      dplyr::filter(!str_detect(subjID, "pilot")) %>%  # remove rows with pilot participants
      dplyr::filter(!str_detect(subjID, "99901")) %>%  # remove rows with pilot participants
      dplyr::filter(!str_detect(subjID, "98801")) %>%  # remove rows with pilot participants
      dplyr::mutate(results = trimws(results)) %>%
      dplyr::filter(results != "")

    # Add trial column grouped by subject and remove leading semicolon
    dd_temp %<>%
      dplyr::group_by(subjID, time) %>%
      dplyr::mutate(trial = row_number()) %>%
      dplyr::select(subjID, time, trial, results) %>%
      dplyr::mutate(results = ifelse(str_starts(results, ";"), str_replace(results, "^;", ""), results))

    # Break up results information into different columns
    dd_temp %<>%
      separate(results, into = c("placehold1", "placehold2",
                                 "delay", "offer", "response", "response_time"), sep = ";") %>%
      dplyr::select(subjID, trial, delay, offer, response, response_time)

    # ungroup data
    dd_temp %<>% ungroup()

    # Process columns to remove unnecessary characters in trial results
    dd_temp %<>%
      dplyr::mutate(subjID = as.numeric(subjID)) %>%
      dplyr::mutate(trial = as.numeric(trial)) %>%
      dplyr::mutate(delay = as.numeric(str_replace(delay, "qv:", ""))) %>%
      dplyr::mutate(offer = as.numeric(str_replace(offer, "am:", ""))) %>%
      dplyr::mutate(response = as.character(str_replace(response, "an:", ""))) %>%
      dplyr::mutate(response_time = as.numeric(str_replace(response_time, "rt:", "")))

    # Format subj to match project coding of participant identifier (3 + familyid + childid)
    dd_temp %<>%
      dplyr::mutate(subjID = substr(subjID, start = 3, stop = nchar(subjID))) %>%
      dplyr::mutate(subjID = as.numeric(paste0("3", subjID)))

    ## hBayesDM formatting ---------------------------------------------------------------------------------------------------- ##

    dd_temp %<>%
      dplyr::rename(delay_later = delay, amount_sooner = offer, choice = response) %>%
      dplyr::mutate(amount_later = 10, delay_sooner = 0,
             choice = ifelse(choice == "standard", 1, ifelse(choice == "variable", 0, NA))) %>% #0 = immediate, 1 = delayed
      dplyr::select(subjID, trial, delay_later, amount_later, delay_sooner, amount_sooner, choice) %>%
      dplyr::filter(substr(subjID, nchar(subjID) - 1, nchar(subjID)) %in% c("01", "02"))

    # order dataframe in order of subject ID
    dd_temp <- dd_temp[order(dd_temp$subjID), ]

    # remove duplicate entries for participants
    dd_temp %<>%
      dplyr::group_by(subjID) %>%
      mutate(duplicated_trial = duplicated(trial)) %>%
      dplyr::filter(!duplicated_trial) %>%
      dplyr::select(-duplicated_trial)

    # Save to list for storage
    output_list[[file_path]] <- dd_temp
  }

  return(output_list)
}

## PERFORM DATA CLEANING ====================================================================================================

# list csv files to process
dd_file_paths <- c("ses_w02_delaydiscounting.csv", "ses_w03_delaydiscounting.csv", "ses_w04_delaydiscounting.csv",
                   "ses_w05_delaydiscounting.csv", "ses_w06_delaydiscounting.csv")

# process multiple CSV files
dd_output <- dd_preprocess(dd_file_paths)

### Visualisation -----------------------------------------------------------------------------------------------------------

# inspect trial distributions for each measurement wave
den_plot1 <- lapply(dd_output, function(x) ggpubr::ggdensity(x, "trial"))
ggpubr::ggarrange(plotlist = den_plot1, nrow = 2, ncol = 3, label.x = .5,
          labels = c("wave2", "wave3", "wave4", "wave5", "wave6"))

# inspect choice distribution for each measurement wave
den_plot2 <- lapply(dd_output, function(x) ggpubr::ggdensity(x, "choice"))
ggpubr::ggarrange(plotlist = den_plot2, nrow = 2, ncol = 3, label.x = .25,
          labels = c("wave2", "wave3", "wave4", "wave5", "wave6"))

# Get minimum number of trials (per participant, per wave)
max_trials <- lapply(dd_output, function(df) {
  df %>%
    dplyr::group_by(subjID) %>%
    dplyr::summarise(max_trial = max(trial, na.rm = TRUE))
})

# remove participants with fewer than 20 trials
filtered_dd_output <- lapply(dd_output, function(df) {
  incomplete_subjID <- df %>%
    dplyr::group_by(subjID) %>%
    dplyr::summarise(num_trials = max(trial)) %>%
    dplyr::filter(num_trials < 20) %>%
    pull(subjID)

  # exclude those subjID from the dataframe
  df %>%
    dplyr::filter(!subjID %in% incomplete_subjID)
})

# remove participants with no behavioural variation
filtered_dd_output <- lapply(filtered_dd_output, function(df) {

  # Identify the subjID that meet the criteria
  invariant_subjID <- df %>%
    dplyr::group_by(subjID) %>%
    dplyr::summarise(choice_var = sd(choice, na.rm = TRUE)) %>%
    dplyr::filter(choice_var == 0) %>%
    pull(subjID)

  # Remove invariant subjects
  df %>%
    dplyr::filter(!subjID %in% invariant_subjID)
})

#### Excluded participants per wave -----------------------------------------------------------------------------------------
purrr::map_int(dd_output, ~ n_distinct(.x$subjID)) - purrr::map_int(filtered_dd_output, ~ n_distinct(.x$subjID))

# save data
saveRDS(filtered_dd_output, file = here::here("data", "processed", "dd_task.Rds"))
