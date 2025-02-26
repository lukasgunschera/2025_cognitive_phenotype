## ============================================================================================================================ ##
## Script:    MPLUS Preparation
## ============================================================================================================================ ##
## Authors:   Lukas J. Gunschera
## Date:      Wed May  8 08:06:50 2024
## ============================================================================================================================ ##
##
## ============================================================================================================================ ##

rm(list = ls()); set.seed(777)

required_packages <- c("here","dplyr","magrittr","purrr","readr","stringr","haven","tidyr","MplusAutomation")
invisible(lapply(required_packages, library, character.only = TRUE))

here::i_am("README.md")     # set parent directory to README.md file of project
here::here()

source(here("code", "lcid_delaydiscount", "functions", "fun_prepare_mplus_data.R"))

# LOAD DATA -----------------------------------------------------------------------------------------------------------------
dat_tinvar <- readr::read_csv(here("data", "lcid", "dd_delaydiscount", "processed", "tinvar_masterdat_wide.csv"))
dat_tvar   <- readr::read_csv(here("data", "lcid", "dd_delaydiscount", "processed", "tvar_masterdat_wide.csv"))

aux_variables <- readRDS(here::here("data", "lcid", "dd_delaydiscount", "processed", "auxiliary_variables.rds"))
aux_variables

# TIME-INVARIANT DATA -------------------------------------------------------------------------------------------------------
dat_tinvar %<>%
  dplyr::rename(
    # Delay Discounting Parameter Estimates
    logk      = logk,
    logk_low  = logk_hdi_lower,
    logk_high = logk_hdi_upper,
    k         = estimate_k,
    b         = estimate_beta,
    k_low     = hdi_lower_k,
    k_hig     = hdi_upper_k,
    b_low     = hdi_lower_beta,
    b_hig     = hdi_upper_beta,

    # Social Media Total Scores
    smt5  = w05_sm_total,
    smt6  = w06_sm_total,
    smt7  = w07_sm_total,

    # Social Media 'Post and Scrolling'
    smp5  = w05_sm_postandscroll,
    smp6  = w06_sm_postandscroll,
    smp7  = w07_sm_postandscroll,

    # Social Media 'Messaging'
    smm5  = w05_sm_messaging,
    smm6  = w06_sm_messaging,
    smm7  = w07_sm_messaging,

    # Social Media 'Video watching'
    smv5  = w05_sm_video,
    smv6  = w06_sm_video,
    smv7  = w07_sm_video,

    # Early Adolescence Temperament Quesionnaire (EATQ-EC)
    eat5  = w05_eatq_ec_total,
    eat6  = w06_eatq_ec_total,
    eat7  = w07_eatq_ec_total,

    # Behavioural Inhibition and Activation Scale (BIS/BAS)
    bis5  = w05_bisbas_bis,
    bis6  = w06_bisbas_bis,
    bis7  = w07_bisbas_bis,

    bas5  = w05_bisbas_bas,
    bas6  = w06_bisbas_bas,
    bas7  = w07_bisbas_bas,

    bbt5  = w05_bisbas_total,
    bbt6  = w06_bisbas_total,
    bbt7  = w07_bisbas_total,

    # Highly Sensitive Child Scale (HSCS)
    hsc5  = w05_hscs_total,
    hsc6  = w06_hscs_total,
    hsc7  = w07_hscs_total,

    # Strenghts and Difficulties Questionnaire (SDQ)
    sdq5  = w05_sdq_total,
    sdq6  = w06_sdq_total,
    sdq7  = w07_sdq_total,

    # Strengths and Difficulties Questionnaire Externalising & Internalising
    sdqi5 = w05_sdq_int,
    sdqi6 = w06_sdq_int,
    sdqi7 = w07_sdq_int,
    sdqe5 = w05_sdq_ext,
    sdqe6 = w06_sdq_ext,
    sdqe7 = w07_sdq_ext,

    # Demographics
    sex   = w05_sex,
    age5  = w05_age,
    age6  = w06_age,
    age7  = w07_age,
    eth   = ethnicity_c,

    # Auxiliary Variables
    csm5 = w05_cius_total,
    csm6 = w06_cius_total,

    sex = w05_sex,
    ses = ses,
    edu1 = w01_education_op,
    bsi6 = w06_bsi_total,
    bsi7 = w07_bsi_total,
    bsid6 = w06_bsi_depression,
    bsid7 = w07_bsi_depression,
    bsia6 = w06_bsi_anxiety,
    bsia7 = w07_bsi_anxiety,

    pds5 = w05_pds_total,
    pds6 = w06_pds_total,
    pds7 = w07_pds_total

  ) %>%

  dplyr::mutate(

    # Pubertal Development Scale (PDS)
    pdt5  = w05_pds_1 + w05_pds_2 + w05_pds_3,
    pdt6  = w06_pds_1 + w06_pds_2 + w06_pds_3,
    pdt7  = w07_pds_1 + w07_pds_2 + w07_pds_3,
    pdsi2 = w07_pds_2,
  )

dat_tinvar %<>%
  dplyr::select(
    subjID, sex, logk, k, b, logk_low, logk_high, k_low, k_hig, b_low, b_hig,

    smt5, smt6, smt7,
    smp5, smp6, smp7,
    smm5, smm6, smm7,
    smv5, smv6, smv7,
    eat5, eat6, eat7,
    bis5, bis6, bis7,
    bas5, bas6, bas7,
    bbt5, bbt6, bbt7,
    hsc5, hsc6, hsc7,
    sdq5, sdq6, sdq7,
    sdqi5, sdqi6, sdqi7,
    sdqe5, sdqe6, sdqe7,
    pdt5, pdt6, pdt7,
    age5, age6, age7,

    # Auxiliary variables
    csm5, csm6, sex, ses, edu1, bsi6, bsi7, bsid6, bsid7, bsia6, bsia7, pdsi2, pds5, pds6, pds7
  )

# Get ids for subjects who are missing data across the board but sex and subjID
subjnas <- dat_tinvar %>%
  dplyr::filter(rowSums(is.na(.[, -(1:2)])) == ncol(.) - 2) %>%
  dplyr::select(subjID)

# Filter dataframe based on above index
dat_tinvar %<>%
  dplyr::filter(!(subjID %in% subjnas$subjID))


# TIME-VARIANT DATA -------------------------------------------------------------------------------------------------------
dat_tvar %<>%
  dplyr::rename(

    # Delay Discounting log k transformation
    logk2 = w02_logk,
    logk3 = w03_logk,
    logk4 = w04_logk,
    logk5 = w05_logk,
    logk6 = w06_logk,
    logk2_low = w02_logk_hdi_lower,
    logk3_low = w03_logk_hdi_lower,
    logk4_low = w04_logk_hdi_lower,
    logk5_low = w05_logk_hdi_lower,
    logk6_low = w06_logk_hdi_lower,
    logk2_hig = w02_logk_hdi_upper,
    logk3_hig = w03_logk_hdi_upper,
    logk4_hig = w04_logk_hdi_upper,
    logk5_hig = w05_logk_hdi_upper,
    logk6_hig = w06_logk_hdi_upper,

    # Delay Discounting Parameter Estimates
    k02   = w02_estimate_k,
    k03   = w03_estimate_k,
    k04   = w04_estimate_k,
    k05   = w05_estimate_k,
    k06   = w06_estimate_k,
    beta2   = w02_estimate_beta,
    beta3   = w03_estimate_beta,
    beta4   = w04_estimate_beta,
    beta5   = w05_estimate_beta,
    beta6   = w06_estimate_beta,
    k02_low = w02_hdi_lower_k,
    k03_low = w03_hdi_lower_k,
    k04_low = w04_hdi_lower_k,
    k05_low = w05_hdi_lower_k,
    k06_low = w06_hdi_lower_k,
    k02_hig = w02_hdi_upper_k,
    k03_hig = w03_hdi_upper_k,
    k04_hig = w04_hdi_upper_k,
    k05_hig = w05_hdi_upper_k,
    k06_hig = w06_hdi_upper_k,
    beta2_low = w02_hdi_lower_beta,
    beta3_low = w03_hdi_lower_beta,
    beta4_low = w04_hdi_lower_beta,
    beta5_low = w05_hdi_lower_beta,
    beta6_low = w06_hdi_lower_beta,
    beta2_hig = w02_hdi_upper_beta,
    beta3_hig = w03_hdi_upper_beta,
    beta4_hig = w04_hdi_upper_beta,
    beta5_hig = w05_hdi_upper_beta,
    beta6_hig = w06_hdi_upper_beta,

    # Social Media Total Scores
    smt5  = w05_sm_total,
    smt6  = w06_sm_total,
    smt7  = w07_sm_total,

    # Social Media 'Post and Scrolling'
    smp5  = w05_sm_postandscroll,
    smp6  = w06_sm_postandscroll,
    smp7  = w07_sm_postandscroll,

    # Social Media 'Messaging'
    smm5  = w05_sm_messaging,
    smm6  = w06_sm_messaging,
    smm7  = w07_sm_messaging,

    # Social Media 'Video watching'
    smv5  = w05_sm_video,
    smv6  = w06_sm_video,
    smv7  = w07_sm_video,

    # Early Adolescence Temperament Quesionnaire (EATQ-EC)
    eat5  = w05_eatq_ec_total,
    eat6  = w06_eatq_ec_total,
    eat7  = w07_eatq_ec_total,

    # Behavioural Inhibition and Activation Scale (BIS/BAS)
    bis5  = w05_bisbas_bis,
    bis6  = w06_bisbas_bis,
    bis7  = w07_bisbas_bis,

    bas5  = w05_bisbas_bas,
    bas6  = w06_bisbas_bas,
    bas7  = w07_bisbas_bas,

    bbt5  = w05_bisbas_total,
    bbt6  = w06_bisbas_total,
    bbt7  = w07_bisbas_total,

    # Highly Sensitive Child Scale (HSCS)
    hsc5  = w05_hscs_total,
    hsc6  = w06_hscs_total,
    hsc7  = w07_hscs_total,

    # Strenghts and Difficulties Questionnaire (SDQ)
    sdq5  = w05_sdq_total,
    sdq6  = w06_sdq_total,
    sdq7  = w07_sdq_total,

    # Strengths and Difficulties Questionnaire Externalising & Internalising
    sdqi5 = w05_sdq_int,
    sdqi6 = w06_sdq_int,
    sdqi7 = w07_sdq_int,
    sdqe5 = w05_sdq_ext,
    sdqe6 = w06_sdq_ext,
    sdqe7 = w07_sdq_ext,

    # Demographics
    sex   = w05_sex,
    age5  = w05_age,
    age6  = w06_age,
    age7  = w07_age,
    eth   = ethnicity_c,

    # Auxiliary Variables
    csm5 = w05_cius_total,
    csm6 = w06_cius_total,

    sex = w05_sex,
    ses = ses,
    edu1 = w01_education_op,
    bsi6 = w06_bsi_total,
    bsi7 = w07_bsi_total,
    bsid6 = w06_bsi_depression,
    bsid7 = w07_bsi_depression,
    bsia6 = w06_bsi_anxiety,
    bsia7 = w07_bsi_anxiety,

    pds5 = w05_pds_total,
    pds6 = w06_pds_total,
    pds7 = w07_pds_total

  ) %>%
  dplyr::mutate(
    # Pubertal Development Scale (PDS)
    pdt5  = w05_pds_1 + w05_pds_2 + w05_pds_3,
    pdt6  = w06_pds_1 + w06_pds_2 + w06_pds_3,
    pdt7  = w07_pds_1 + w07_pds_2 + w07_pds_3,
    pdsi2 = w07_pds_2,
  )

dat_tvar %<>%
  dplyr::select(
    subjID, sex,

    logk2, logk3, logk4, logk5, logk6,
    logk2_low, logk3_low, logk4_low, logk5_low, logk6_low,
    logk2_hig, logk3_hig, logk4_hig, logk5_hig, logk6_hig,

    k02, k03, k04, k05, k06,
    beta2, beta3, beta4, beta5, beta6,
    k02_low, k03_low, k04_low, k05_low, k06_low,
    k02_hig, k03_hig, k04_hig, k05_hig, k06_hig,
    beta2_low, beta3_low, beta4_low, beta5_low, beta6_low,
    beta2_hig, beta3_hig, beta4_hig, beta5_hig, beta6_hig,
    smt5, smt6, smt7,
    smp5, smp6, smp7,
    smm5, smm6, smm7,
    smv5, smv6, smv7,
    eat5, eat6, eat7,
    bis5, bis6, bis7,
    bas5, bas6, bas7,
    bbt5, bbt6, bbt7,
    hsc5, hsc6, hsc7,
    sdq5, sdq6, sdq7,
    sdqi5, sdqi6, sdqi7,
    sdqe5, sdqe6, sdqe7,
    pdt5, pdt6, pdt7,
    age5, age6, age7,

    # Auxiliary Variables
    csm5, csm6, sex, ses, edu1, bsi6, bsi7, bsid6, bsid7, bsia6, bsia7, pdsi2, pds5, pds6, pds7
  )

# Get ids for subjects who are missing data across the board but sex and subjID
subjnas <- dat_tvar %>%
  dplyr::filter(rowSums(is.na(.[, -(1:2)])) == ncol(.) - 2) %>%
  dplyr::select(subjID)

# Filter dataframe based on above index
dat_tvar %<>%
  dplyr::filter(!(subjID %in% subjnas$subjID))


# MPLUS DATA PREPARATION ----------------------------------------------------------------------------------------------------
# MPLUS cannot handle data that have missing values across the relevant columns, this function removes said participants
mplus_analysis_filter_na <- function(df, varnames, exclude = "sex") {

  # Get index of subjects who have at least one non-NA value in the specified columns
  indx_filtered <- df %>%
    dplyr::select(all_of(varnames)) %>%
    # Filter out rows where all the columns except subjID and sex are NA
    dplyr::filter(rowSums(!is.na(dplyr::select(., -subjID, -sex, -all_of(exclude)))) > 0) %>%
    dplyr::select(subjID)

  # Select only the columns that are needed for the analysis, replace NA with -999 for analysis in MPLUS
  df %<>%
    dplyr::filter(subjID %in% indx_filtered$subjID) %>%
    dplyr::mutate_all(~coalesce(., -999))

  return(df)
}


aux_variables <- c('csm5', 'csm6', 'sex', 'ses', 'edu1',
                   'bsi6', 'bsi7', 'bsid6', 'bsid7', 'bsia6', 'bsia7', 'pdsi2', 'pds5', 'pds6', 'pds7')

## Create data subsets for baseline models  ----------------------------------------------------------------------------------- ##
dat_baseline_eatq <-
  mplus_analysis_filter_na(df = dat_tinvar, varnames = c("subjID", "sex", "smp5", "smp6", "smp7", "eat5", "eat6", "eat7"))
dat_baseline_bisbas <-
  mplus_analysis_filter_na(df = dat_tinvar, varnames = c("subjID", "sex", "smp5", "smp6", "smp7", "bbt5", "bbt6", "bbt7"))
dat_baseline_bis <-
  mplus_analysis_filter_na(df = dat_tinvar, varnames = c("subjID", "sex", "smp5", "smp6", "smp7", "bis5", "bis6", "bis7"))
dat_baseline_bas <-
  mplus_analysis_filter_na(df = dat_tinvar, varnames = c("subjID", "sex", "smp5", "smp6", "smp7", "bas5", "bas6", "bas7"))
dat_baseline_hscs <-
  mplus_analysis_filter_na(df = dat_tinvar, varnames = c("subjID", "sex", "smp5", "smp6", "smp7", "hsc5", "hsc6", "hsc7"))
dat_baseline_sdq <-
  mplus_analysis_filter_na(df = dat_tinvar, varnames = c("subjID", "sex", "smp5", "smp6", "smp7", "sdq5", "sdq6", "sdq7"))
dat_baseline_sdqi <-
  mplus_analysis_filter_na(df = dat_tinvar, varnames = c("subjID", "sex", "smp5", "smp6", "smp7", "sdqi5", "sdqi6", "sdqi7"))
dat_baseline_sdqe <-
  mplus_analysis_filter_na(df = dat_tinvar, varnames = c("subjID", "sex", "smp5", "smp6", "smp7", "sdqe5", "sdqe6", "sdqe7"))

## Create data subsets for time-invariant moderation models  ------------------------------------------------------------------ ##
dat_mod_eatq <-
  mplus_analysis_filter_na(df = dat_tinvar, exclude = "logk",
                           varnames = c("subjID", "logk", "sex", "smp5", "smp6", "smp7", "eat5", "eat6", "eat7"))
dat_mod_bisbas <-
  mplus_analysis_filter_na(df = dat_tinvar, exclude = "logk",
                           varnames = c("subjID", "logk", "sex", "smp5", "smp6", "smp7", "bbt5", "bbt6", "bbt7"))
dat_mod_hscs <-
  mplus_analysis_filter_na(df = dat_tinvar, exclude = "logk",
                           varnames = c("subjID", "logk", "sex", "smp5", "smp6", "smp7", "hsc5", "hsc6", "hsc7"))
dat_mod_sdq <-
  mplus_analysis_filter_na(df = dat_tinvar, exclude = "logk",
                           varnames = c("subjID", "logk", "sex", "smp5", "smp6", "smp7", "sdq5", "sdq6", "sdq7"))
dat_mod_sdqi <-
  mplus_analysis_filter_na(df = dat_tinvar, exclude = "logk",
                           varnames = c("subjID", "logk", "sex", "smp5", "smp6", "smp7", "sdqi5", "sdqi6", "sdqi7"))
dat_mod_sdqe <-
  mplus_analysis_filter_na(df = dat_tinvar, exclude = "logk",
                           varnames = c("subjID", "logk", "sex", "smp5", "smp6", "smp7", "sdqe5", "sdqe6", "sdqe7"))

## Create data subsets for time-variant moderation models  ------------------------------------------------------------------ ##
dat_logk_eatq <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "logk5", "logk6", "sex", "smp5", "smp6", "smp7", "eat5", "eat6", "eat7"), exclude = c("logk5","logk6"))
dat_logk_bisbas <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "logk5", "logk6", "sex", "smp5", "smp6", "smp7", "bbt5", "bbt6", "bbt7"), exclude = c("logk5","logk6"))
dat_logk_bis <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "logk5", "logk6", "sex", "smp5", "smp6", "smp7", "bis5", "bis6", "bis7"), exclude = c("logk5","logk6"))
dat_logk_bas <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "logk5", "logk6", "sex", "smp5", "smp6", "smp7", "bas5", "bas6", "bas7"), exclude = c("logk5","logk6"))
dat_logk_hscs <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "logk5", "logk6", "sex", "smp5", "smp6", "smp7", "hsc5", "hsc6", "hsc7"), exclude = c("logk5","logk6"))
dat_logk_sdq <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "logk5", "logk6", "sex", "smp5", "smp6", "smp7", "sdq5", "sdq6", "sdq7"), exclude = c("logk5","logk6"))
dat_logk_sdqi <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "logk5", "logk6", "sex", "smp5", "smp6", "smp7", "sdqi5", "sdqi6", "sdqi7"), exclude = c("logk5","logk6"))
dat_logk_sdqe <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "logk5", "logk6", "sex", "smp5", "smp6", "smp7", "sdqe5", "sdqe6", "sdqe7"), exclude = c("logk5","logk6"))

## Inverse Temperature BETA =================================================================================================== ##

## Create data subsets for time-variant moderation models  -------------------------------------------------------------------- ##
dat_beta_eatq <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "beta5", "beta6", "sex", "smp5", "smp6", "smp7", "eat5", "eat6", "eat7"), exclude = c("beta5","beta6"))
dat_beta_bisbas <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "beta5", "beta6", "sex", "smp5", "smp6", "smp7", "bbt5", "bbt6", "bbt7"), exclude = c("beta5","beta6"))
dat_beta_bis <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "beta5", "beta6", "sex", "smp5", "smp6", "smp7", "bis5", "bis6", "bis7"), exclude = c("beta5","beta6"))
dat_beta_bas <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "beta5", "beta6", "sex", "smp5", "smp6", "smp7", "bas5", "bas6", "bas7"), exclude = c("beta5","beta6"))
dat_beta_hscs <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "beta5", "beta6", "sex", "smp5", "smp6", "smp7", "hsc5", "hsc6", "hsc7"), exclude = c("beta5","beta6"))
dat_beta_sdq <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "beta5", "beta6", "sex", "smp5", "smp6", "smp7", "sdq5", "sdq6", "sdq7"), exclude = c("beta5","beta6"))
dat_beta_sdqi <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "beta5", "beta6", "sex", "smp5", "smp6", "smp7", "sdqi5", "sdqi6", "sdqi7"), exclude = c("beta5","beta6"))
dat_beta_sdqe <-
  mplus_analysis_filter_na(df = dat_tvar, varnames = c("subjID", "beta5", "beta6", "sex", "smp5", "smp6", "smp7", "sdqe5", "sdqe6", "sdqe7"), exclude = c("beta5","beta6"))

# DETERMINE INCLUDED VARIABLES ---------------------------------------------------------------------------------------------

# Column names that are included in all MPLUS datasets
common_cols <- c("smt5","smt6","smt7","smp5","smp6","smp7","smm5","smm6","smm7","smv5","smv6","smv7",
                 "eat5","eat6","eat7","bis5","bis6","bis7", "bas5","bas6","bas7","bbt5","bbt6","bbt7",
                 "hsc5","hsc6","hsc7", "sdq5","sdq6","sdq7","sdqi5","sdqi6","sdqi7","sdqe5","sdqe6","sdqe7",
                 # Auxiliary variables
                 aux_variables)

# Columns that are added for the moderation analyses MPLUS datasets
tvar_cols <- c("logk2", "logk3", "logk4", "logk5", "logk6")
tvar_cols_beta <- c("beta2", "beta3", "beta4", "beta5", "beta6")

# Columns that are added for the time-invariant moderation analyses MPLUS datasets
tinvar_col <- "logk"
tinvar_col_beta <- "b"

# Baseline datasets
baseline_datasets <- list(
  list(df = dat_baseline_eatq, filename = "lcid_baseline_eatq.dat"),
  list(df = dat_baseline_bisbas, filename = "lcid_baseline_bisbas.dat"),
  list(df = dat_baseline_bis, filename = "lcid_baseline_bis.dat"),
  list(df = dat_baseline_bas, filename = "lcid_baseline_bas.dat"),
  list(df = dat_baseline_hscs, filename = "lcid_baseline_hscs.dat"),
  list(df = dat_baseline_sdq, filename = "lcid_baseline_sdq.dat"),
  list(df = dat_baseline_sdqi, filename = "lcid_baseline_sdqi.dat"),
  list(df = dat_baseline_sdqe, filename = "lcid_baseline_sdqe.dat")
)

# Tinvar  moderation datasets
tinvar_datasets <- list(
  list(df = dat_mod_eatq, filename = "lcid_tinvar_logk_eatq.dat"),
  list(df = dat_mod_bisbas, filename = "lcid_tinvar_logk_bisbas.dat"),
  list(df = dat_mod_hscs, filename = "lcid_tinvar_logk_hscs.dat"),
  list(df = dat_mod_sdq, filename = "lcid_tinvar_logk_sdq.dat"),
  list(df = dat_mod_sdqi, filename = "lcid_tinvar_logk_sdqi.dat"),
  list(df = dat_mod_sdqe, filename = "lcid_tinvar_logk_sdqe.dat")
)

# Tvar moderation (logk) datasets
tvar_datasets <- list(
  list(df = dat_logk_eatq, filename = "lcid_logk_eatq.dat"),
  list(df = dat_logk_bisbas, filename = "lcid_logk_bisbas.dat"),
  list(df = dat_logk_bis, filename = "lcid_logk_bis.dat"),
  list(df = dat_logk_bis, filename = "lcid_logk_bas.dat"),
  list(df = dat_logk_hscs, filename = "lcid_logk_hscs.dat"),
  list(df = dat_logk_sdq, filename = "lcid_logk_sdq.dat"),
  list(df = dat_logk_sdqi, filename = "lcid_logk_sdqi.dat"),
  list(df = dat_logk_sdqe, filename = "lcid_logk_sdqe.dat")
)

# Tvar moderation (beta) datasets
tvar_datasets_beta <- list(
  list(df = dat_beta_eatq, filename = "lcid_beta_eatq.dat"),
  list(df = dat_beta_bisbas, filename = "lcid_beta_bisbas.dat"),
  list(df = dat_beta_bis, filename = "lcid_beta_bis.dat"),
  list(df = dat_beta_bis, filename = "lcid_beta_bas.dat"),
  list(df = dat_beta_hscs, filename = "lcid_beta_hscs.dat"),
  list(df = dat_beta_sdq, filename = "lcid_beta_sdq.dat"),
  list(df = dat_beta_sdqi, filename = "lcid_beta_sdqi.dat"),
  list(df = dat_beta_sdqe, filename = "lcid_beta_sdqe.dat")
)

 # PROCESS MPLUS DATA --------------------------------------------------------------------------------------------------------

# Process baseline datasets
lapply(baseline_datasets, function(dataset) {
  prepare_mplus_data(dataset$df, dataset$filename, common_cols, foldername = "dd_delaydiscount")
})

# Process tinvar moderation datasets
lapply(tinvar_datasets, function(dataset) {
  prepare_mplus_data(dataset$df, dataset$filename, c(tinvar_col, common_cols), foldername = "dd_delaydiscount")
})

# Process tvar moderation (logk) datasets
lapply(tvar_datasets, function(dataset) {
  prepare_mplus_data(dataset$df, dataset$filename, c(tvar_cols, common_cols), foldername = "dd_delaydiscount")
})

# Process tvar moderation (beta) datasets
lapply(tvar_datasets_beta, function(dataset) {
  prepare_mplus_data(dataset$df, dataset$filename, c(tvar_cols_beta, common_cols), foldername = "dd_delaydiscount")
})

