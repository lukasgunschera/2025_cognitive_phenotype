## ======================================================================================================================= ##
## Script:       PREPROCESS LCID DATAFILES
## ======================================================================================================================= ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2024-07-11
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

# recreate package environment
library(renv)
renv::restore()

set.seed(777)

library(ragg)
library(here)
library(dplyr)
library(haven)
library(tidyr)
library(readr)
library(ggpubr)
library(janitor)
library(stringr)
library(ggplot2)
library(viridis)
library(cowplot)
library(magrittr)
library(ggthemes)
library(extrafont)

# set root
here::i_am("renv.lock")

# source functions
source(here::here("code", "functions", "fun_plots.R"))
source(here::here("code", "functions", "fun_helper.R"))

## PROCESS RAW DATA =========================================================================================================
# Coding conventions in LCID dataset
# 0 = false, 1 = true
# 0 = male, 1 = female
# 999 = missing identifier
# 998801 and 99901 pilot participant identifiers

### Demographics ------------------------------------------------------------------------------------------------------------

dat_demographics <- haven::read_sav(here("data", "raw", "mcc_demographics.sav"))

# create consistent subject identifier
dat_demographics %<>%
  dplyr::mutate(subjID = as.double(gsub("^mcc", "3", subjectid))) %>%
  dplyr::select(subjID, everything(), -cohort, -childnr, -familyid, -subjectid, -dplyr::contains("visitid"))

# indicate missing values
dat_demographics %<>% dplyr::mutate(across(where(is.numeric), ~ ifelse(. == 999, NA, .)))

# remove "mcc_" from columnnames
dat_demographics %<>% dplyr::rename_with(~ str_replace(., "^mcc_", ""), starts_with("mcc_"))

dat_demographics %<>%
  dplyr::filter(str_length(subjID) == 7) %>% # remove rows with invalid participant IDs
  dplyr::filter(!str_detect(subjID, "pilot")) %>% # remove rows with pilot participants
  dplyr::filter(!str_detect(subjID, "99901")) %>% # remove rows with pilot participants
  dplyr::filter(!str_detect(subjID, "98801")) # remove rows with pilot participants

#### Save processed demographic data ----------------------------------------------------------------------------------------

readr::write_csv(dat_demographics, here("data", "processed", "demographics.csv"))

### Questionnaires ----------------------------------------------------------------------------------------------------------

# load raw questionnaire data of relevant waves 5 to 7
dat_q05 <- haven::read_sav(here("data", "raw", "mcc_ses_w05_questionnaires.sav"))
dat_q06 <- haven::read_sav(here("data", "raw", "mcc_ses_w06_questionnaires.sav"))
dat_q07 <- haven::read_sav(here("data", "raw", "mcc_ses_w07_questionnaires.sav"))

#### Data processing function -----------------------------------------------------------------------------------------------
process_questionnaires <- function(df) {
  # set wave value for later wave-dependent processing
  if (any(grepl("5", df$Wave))) {
    wave_value <- 5
  } else if (any(grepl("6", df$Wave))) {
    wave_value <- 6
  } else if (any(grepl("7", df$Wave))) {
    wave_value <- 7
  }

  # recode missing identifiers as NA
  df %<>%
    dplyr::mutate(across(where(is.numeric) & !dplyr::contains("ChildID") & !dplyr::contains("FamilyID") &
      !dplyr::contains("ChildID"), ~ ifelse(. == 999, NA, .))) %>%
    dplyr::mutate(across(where(is.numeric) & !dplyr::contains("ChildID") & !dplyr::contains("FamilyID") &
      !dplyr::contains("ChildID"), ~ ifelse(. == 888, NA, .)))

  df %<>%
    dplyr::mutate(
      FamilyID = sprintf("%04d", FamilyID), # fill to four digits with leading zeros
      ChildNr = sprintf("%02d", ChildNr), # fill to two digits with leading zeros
      FamilyID = paste0("3", FamilyID), # prepend with three for middle child cohort
      subjID = paste0(FamilyID, ChildNr), # combine child and family id for subject id
      subjID = as.double(subjID), # convert to double
      across(where(is.numeric), ~ ifelse(. == 999, NA, .)) # recode missing identifier as NA
    ) %>%
    dplyr::select(subjID, everything()) # deselect unnecessary columns

  df %<>%
    dplyr::filter(str_length(subjID) == 7) %>% # remove rows with invalid participant IDs
    dplyr::filter(!str_detect(subjID, "pilot")) %>% # remove rows with pilot participants
    dplyr::filter(!str_detect(subjID, "99901")) %>% # remove rows with pilot participants
    dplyr::filter(!str_detect(subjID, "98801")) # remove rows with pilot participants

  df %<>%
    dplyr::rename_with(~ stringr::str_replace(., "^mcc_", ""), dplyr::starts_with("mcc_")) %>% # remove leading mcc
    dplyr::rename_with(~ stringr::str_replace(., "^MCC_", ""), dplyr::starts_with("MCC_")) # remove leading MCC

  #### EATQ-EC Subscale -----------------------------------------------------------------------------------------------------

  if (wave_value == 7) { # EATQ Formatting Wave 7

    # items to be reversed
    eatq_reverse_coded <- c(
      "w07_EATQ_Q4_EffAtt_7", "w07_EATQ_Q4_EffInh_3", "w07_EATQ_Q4_EffInh_8",
      "w07_EATQ_Q4_EffInh_9", "w07_EATQ_Q4_EffInh_11"
    )

    df %<>%
      dplyr::mutate(across(all_of(dplyr::contains(eatq_reverse_coded)), ~ reverse_code(.))) %>% # reverse code relevant items
      dplyr::mutate(eatq_ec_total = rowSums(dplyr::select(., dplyr::contains("EATQ_Q4")), na.rm = FALSE))
  } else { # EATQ Formatting Wave 5 - 6

    # items to be reversed
    eatq_reverse_coded <- c(
      "EATQ_EC_Attention_7_C", "EATQ_EC_Inhibitory_10_C", "EATQ_EC_Inhibitory_15_C",
      "EATQ_EC_Inhibitory_16_C", "EATQ_EC_Inhibitory_18_C"
    )

    df %<>%
      dplyr::mutate(across(dplyr::contains("EATQ_EC"), ~ ifelse(. == 7, 5, .))) %>% # recode maximum
      dplyr::mutate(across(all_of(dplyr::contains(eatq_reverse_coded)), ~ reverse_code(.))) %>% # reverse code relevant items
      dplyr::mutate(eatq_ec_total = rowSums(
        dplyr::select(., dplyr::contains("EATQ_EC") & !dplyr::contains("Wildman")), na.rm = FALSE))

  }

  #### BISBAS scale ---------------------------------------------------------------------------------------------------------

  bisbas_reverse_coded <- c("BISBAS_BIS_22_C", "BISBAS_BIS_2_C")
  # BISBAS Coding following below transformations
  # 1 = strongly disagree
  # 2 = disagree
  # 3 = agree
  # 4 = strongly agree

  if (wave_value == 5) { # BIS/BAS Formatting Wave 5 (Likert scored 1 to 4)

    df %<>%
      dplyr::mutate(
        across(dplyr::contains("BISBAS"), ~ ifelse(. == 1, 4, .)),
        across(dplyr::contains("BISBAS"), ~ ifelse(. == 3, 1, .)),
        across(dplyr::contains("BISBAS"), ~ ifelse(. == 5, 3, .)), # resolve coding error in Qualtrics data
        across(dplyr::contains("BISBAS"), ~ ifelse(. == 2, 2, .)),
        across(all_of(dplyr::contains(bisbas_reverse_coded)), ~ reverse_code(.))
      ) %>%
      mutate(
        bisbas_total = rowSums(dplyr::select(., dplyr::contains("BISBAS") & !dplyr::contains("Filler")), na.rm = FALSE),
        bisbas_bis = rowSums(dplyr::select(., dplyr::contains("BISBAS") &
          dplyr::contains("_BIS_") & !dplyr::contains("Filler")), na.rm = FALSE),
        bisbas_bas = rowSums(dplyr::select(., dplyr::contains("BISBAS") &
          !dplyr::contains("_BIS_") &
          !dplyr::contains("Filler")), na.rm = FALSE)
      )
  } else { # BIS/BAS Formatting Wave 6 to 7 (Likert scored 0 to 3)

    df %<>%
      dplyr::mutate(
        across(dplyr::contains("BISBAS"), ~ ifelse(. == 3, 4, .)), # match Likert scale of fifth wave
        across(dplyr::contains("BISBAS"), ~ ifelse(. == 2, 3, .)), # match Likert scale of fifth wave
        across(dplyr::contains("BISBAS"), ~ ifelse(. == 1, 2, .)), # match Likert scale of fifth wave
        across(dplyr::contains("BISBAS"), ~ ifelse(. == 0, 1, .)), # match Likert scale of fifth wave
        across(all_of(dplyr::contains(bisbas_reverse_coded)), ~ reverse_code(.)) # reverse code relevant items
      ) %>%
      dplyr::mutate(
        bisbas_total = rowSums(dplyr::select(., dplyr::contains("BISBAS") &
          !dplyr::contains("Filler")), na.rm = FALSE),
        bisbas_bis = rowSums(dplyr::select(., dplyr::contains("BISBAS") & dplyr::contains("BIS_") &
          !dplyr::contains("Filler")), na.rm = FALSE),
        bisbas_bas = rowSums(dplyr::select(., dplyr::contains("BISBAS") &
          !dplyr::contains("BIS_") &
          !dplyr::contains("Filler")), na.rm = FALSE)
      )
  }

  #### HSC scale ------------------------------------------------------------------------------------------------------------

  # First, let's ensure we're working with numeric values
  df %<>%
    mutate(across(contains("HSCS"), as.numeric))

  if(wave_value != 6) {
    df %<>%
      # recode only for non-wave 6 data
      mutate(across(contains("HSCS"), ~ ifelse(. >= 5, . - 1, .))) %>%
      # calculate total (now with recoded values)
      mutate(hscs_total = rowSums(select(., contains("HSCS")), na.rm = FALSE))
    } else {
      # for wave 6 - no recoding needed
      df %<>%
        mutate(hscs_total = rowSums(select(., contains("HSCS")), na.rm = FALSE))
    }

  #### SDQ scale -------------------------------------------------------------------------------------------------------------

  # sample vector
  sdq_colnames <- c(
    "w07_sdq_prosocial_q1_c", "w07_sdq_hyper_q2_c", "w07_sdq_emotional_q3_c", "w07_sdq_prosocial_q4_c",
    "w07_sdq_conduct_q5_c", "w07_sdq_peer_q6_c", "w07_sdq_conduct_q7_c", "w07_sdq_emotional_q8_c",
    "w07_sdq_prosocial_q9_c", "w07_sdq_hyper_q10_c", "w07_sdq_peer_q11_c", "w07_sdq_conduct_q12_c",
    "w07_sdq_emotional_q13_c", "w07_sdq_peer_q14_c", "w07_sdq_hyper_q15_c", "w07_sdq_emotional_q16_c",
    "w07_sdq_prosocial_q17_c", "w07_sdq_conduct_q18_c", "w07_sdq_peer_q19_c", "w07_sdq_prosocial_q20_c",
    "w07_sdq_hyper_q21_c", "w07_sdq_conduct_q22_c", "w07_sdq_peer_q23_c", "w07_sdq_emotional_q24_c",
    "w07_sdq_hyper_q25_c"
  )

  if (wave_value == 7) { # SDQ Formatting Wave 7

    df %<>%
      rename_with(~sdq_colnames, .cols = contains("SDQ_Self"))

    # recode for 0 to be the lowest answer option
    df %<>%
      mutate(across(contains(sdq_colnames), ~ . - 1))
  }

  # items to be reversed
  sdq_reverse_coded <- c("sdq_conduct_q7_c", "sdq_peer_q11_c", "sdq_peer_q14_c", "sdq_hyper_q21_c", "sdq_hyper_q25_c")

  df %<>%
    dplyr::mutate(across(all_of(dplyr::contains(sdq_reverse_coded)), ~ reverse_code(.)),
      sdq_total = rowSums(dplyr::select(., dplyr::contains("sdq") & !dplyr::contains("rec")), na.rm = FALSE),
      sdq_ext = rowSums(dplyr::select(., dplyr::contains("sdq_conduct") | dplyr::contains("sdq_hyper")), na.rm = FALSE),
      sdq_int = rowSums(dplyr::select(., dplyr::contains("sdq_emotional") | dplyr::contains("sdq_peer")), na.rm = FALSE)
    )

  #### CIUS scale -----------------------------------------------------------------------------------------------------------

  if(wave_value != 7 ){
    df %<>%
      dplyr::mutate(cius_total = rowSums(dplyr::select(., dplyr::contains("_SocialMedia_C")), na.rm = FALSE)) %>%
      dplyr::mutate(cius_total = ifelse(cius_total == 0, NA, cius_total))
    } else {

      df %<>%
        dplyr::mutate(cius_total = rowSums(
          dplyr::select(., dplyr::contains("_Media_Q15") | dplyr::contains("_Media_Q16")), na.rm = FALSE)) %>%
        dplyr::mutate(cius_total = ifelse(cius_total == 0, NA, cius_total))
  }

  #### Social media measures ------------------------------------------------------------------------------------------------

  if (wave_value == 5) {
    df %<>%
      dplyr::rename(
        w05_sm_video = w05_SocialMedia_B_1_C, # watching video material
        w05_sm_gaming = w05_SocialMedia_B_2_C, # playing games
        w05_sm_postandscroll = w05_SocialMedia_B_6_C, # posting or viewing public messages, photos or videos
        w05_sm_messaging = w05_SocialMedia_B_7_C, # sending or viewing private messages or photos
        w05_sm_videocall = w05_SocialMedia_B_8_C # phone calls or video calls
      ) %>%
      dplyr::mutate( # 0 = none
        w05_sm_video = w05_sm_video - 1, # 1 = 0-1h
        w05_sm_gaming = w05_sm_gaming - 1, # 2 = 1-2h
        w05_sm_postandscroll = w05_sm_postandscroll - 1, # 3 = 2-3h
        w05_sm_messaging = w05_sm_messaging - 1, # 4 = 3-4h
        w05_sm_videocall = w05_sm_videocall - 1, # 5 = 4+h
        w05_sm_total = w05_sm_video + w05_sm_messaging + w05_sm_videocall + w05_sm_postandscroll
      )
  } else if (wave_value == 6) {
    df %<>%
      dplyr::rename(
        w06_sm_video = w06_SocialMedia_B_1_Child, # watching video material
        w06_sm_gaming = w06_SocialMedia_B_2_Child, # playing games
        w06_sm_postandscroll = w06_SocialMedia_B_6_Child, # posting or viewing public messages, photos or videos
        w06_sm_messaging = w06_SocialMedia_B_7_Child, # sending or viewing private messages or photos
        w06_sm_videocall = w06_SocialMedia_B_8_Child # phone calls or video calls
      ) %>%
      dplyr::mutate( # 0 = none
        w06_sm_video = w06_sm_video - 1, # 1 = 0-1h
        w06_sm_gaming = w06_sm_gaming - 1, # 2 = 1-2h
        w06_sm_postandscroll = w06_sm_postandscroll - 1, # 3 = 2-3h
        w06_sm_messaging = w06_sm_messaging - 1, # 4 = 3-4h
        w06_sm_videocall = w06_sm_videocall - 1, # 5 = 4+h
        w06_sm_total = w06_sm_video + w06_sm_messaging + w06_sm_videocall + w06_sm_postandscroll
      )
  } else if (wave_value == 7) {
    df %<>%
      dplyr::rename(
        w07_sm_video = w07_Media_Q5_1, # watching video material
        w07_sm_gaming = w07_Media_Q5_2, # playing games
        w07_sm_postandscroll = w07_Media_Q5_6, # posting or viewing public messages, photos or videos
        w07_sm_messaging = w07_Media_Q5_7, # sending or viewing private messages or photos
        w07_sm_videocall = w07_Media_Q5_8 # phone calls or video calls
      ) %>%
      dplyr::mutate( # 0 = none
        w07_sm_video = w07_sm_video - 1, # 1 = 0-1h
        w07_sm_gaming = w07_sm_gaming - 1, # 2 = 1-2h
        w07_sm_postandscroll = w07_sm_postandscroll - 1, # 3 = 2-3h
        w07_sm_messaging = w07_sm_messaging - 1, # 4 = 3-4h
        w07_sm_videocall = w07_sm_videocall - 1, # 5 = 4+h
        w07_sm_total = w07_sm_video + w07_sm_messaging + w07_sm_videocall + w07_sm_postandscroll
      )
  }

  #### Pubertal development scale -------------------------------------------------------------------------------------------

  if (wave_value == 5) { # PDS Formatting Wave 5

    df %<>%
      dplyr::mutate(
        w05_PDS_1_C = w05_PDS_1_C + 1, # growth spurt
        w05_PDS_2_C = w05_PDS_2_C + 1, # body hair
        w05_PDS_3_C = w05_PDS_3_C + 1, # skin change

        w05_PDS_Boys_4_C = w05_PDS_Boys_4_C + 1, # lower voice
        w05_PDS_Boys_5_C = w05_PDS_Boys_5_C + 1, # facial hair

        w05_PDS_Girls_4_C = w05_PDS_Girls_4_C + 1, # breast development

        w05_PDS_Girls_5_C = case_when( # menarche
          is.na(w05_PDS_Girls_5_C) ~ w05_PDS_Girls_5_C,
          w05_PDS_Girls_5_C == 1 ~ 5,
          w05_PDS_Girls_5_C == 0 ~ 1,
          TRUE ~ w05_PDS_Girls_5_C
        )
      )

    # compute total score
    df %<>%
      rowwise() %>%
      dplyr::mutate(w05_PDS_total = sum(w05_PDS_1_C, w05_PDS_2_C, w05_PDS_3_C, w05_PDS_Boys_4_C, w05_PDS_Boys_5_C,
        w05_PDS_Girls_4_C, w05_PDS_Girls_5_C,
        na.rm = TRUE
      )) %>%
      dplyr::mutate(w05_PDS_total = ifelse(w05_PDS_total == 0, NA, w05_PDS_total)) %>%
      ungroup()
  } else if (wave_value == 6) { # PDS Formatting Wave 6

    df %<>%
      dplyr::mutate(
        w06_PDS_Girls_5_Chi = case_when( # menarche
          is.na(w06_PDS_Girls_5_Chi) ~ w06_PDS_Girls_5_Chi,
          w06_PDS_Girls_5_Chi == 1 ~ 1,
          w06_PDS_Girls_5_Chi == 2 ~ 5,
          TRUE ~ w06_PDS_Girls_5_Chi
        )
      )

    # compute total score
    df %<>%
      rowwise() %>%
      dplyr::mutate(w06_PDS_total = sum(w06_PDS_1_Child, w06_PDS_2_Child, w06_PDS_3_Child, w06_PDS_Boys_4_Chil,
        w06_PDS_Boys_5_Chil, w06_PDS_Girls_4_Chi, w06_PDS_Girls_5_Chi,
        na.rm = TRUE
      )) %>%
      dplyr::mutate(w06_PDS_total = ifelse(w06_PDS_total == 0, NA, w06_PDS_total)) %>%
      ungroup()
  } else { # PDS Formatting Wave 7

    df %<>%
      dplyr::mutate(
        w07_PDS_Q1 = w07_PDS_Q1 - 6, # growth spurt
        w07_PDS_Q2 = w07_PDS_Q2 - 6, # body hair
        w07_PDS_Q3 = w07_PDS_Q3 - 6, # skin change

        w07_PDS_Boys_Q1 = w07_PDS_Boys_Q1 - 6, # lower voice
        w07_PDS_Boys_Q2 = w07_PDS_Boys_Q2 - 6, # facial hair

        w07_PDS_Girls_Q1 = w07_PDS_Girls_Q1 - 6, # breast development


        w07_PDS_Girls_Q2 = case_when( # menarche
          is.na(w07_PDS_Girls_Q2) ~ w07_PDS_Girls_Q2,
          w07_PDS_Girls_Q2 == 16 ~ 1,
          w07_PDS_Girls_Q2 == 17 ~ 5,
          TRUE ~ w07_PDS_Girls_Q2
        )
      )

    # compute total score
    df %<>%
      rowwise() %>%
      dplyr::mutate(w07_PDS_total = sum(w07_PDS_Q1, w07_PDS_Q2, w07_PDS_Q3, w07_PDS_Boys_Q1, w07_PDS_Boys_Q2, w07_PDS_Girls_Q1,
        w07_PDS_Girls_Q2,
        na.rm = TRUE
      )) %>%
      dplyr::mutate(w07_PDS_total = ifelse(w07_PDS_total == 0, NA, w07_PDS_total)) %>%
      ungroup()
  }

  ### Wave 6 and 7 exclusive questionnaires ---------------------------------------------------------------------------------

  if (wave_value >= 6) {
    #### BSI scale ----------------------------------------------------------------------------------------------------------
    # Wildman items = Wildman symptom checklist to assess non-credible symptoms

    df %<>%
      dplyr::mutate(
        bsi_total = rowSums(dplyr::select(., dplyr::contains("BSI") & !dplyr::contains("Wildman")), na.rm = FALSE),
        bsi_depression = rowSums(dplyr::select(., dplyr::contains("BSI") & dplyr::contains("Depression")), na.rm = FALSE),
        bsi_anxiety = rowSums(dplyr::select(., dplyr::contains("BSI") & dplyr::contains("Anxiety")), na.rm = FALSE),
        bsi_sensitivity = rowSums(dplyr::select(., dplyr::contains("BSI") & dplyr::contains("IntSens")), na.rm = FALSE),
        bsi_hostility = rowSums(dplyr::select(., dplyr::contains("BSI") & dplyr::contains("Hostility")), na.rm = FALSE),
        bsi_wildman_check = rowSums(dplyr::select(., dplyr::contains("BSI") & dplyr::contains("Wildman")), na.rm = FALSE),
      )
  }
  return(df)
}

## PROCESS DATA =============================================================================================================

dat_q05_processed <- process_questionnaires(dat_q05)
dat_q06_processed <- process_questionnaires(dat_q06)
dat_q07_processed <- process_questionnaires(dat_q07)

# replace empty character strings with NA
dat_q05_processed %<>%
  dplyr::mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  dplyr::mutate(across(where(is.factor), ~ na_if(as.character(.x), ""))) %>%
  dplyr::mutate(across(where(is.character), as.character)) %>%
  dplyr::mutate(across(where(is.factor), as.factor))

dat_q06_processed %<>%
  dplyr::mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  dplyr::mutate(across(where(is.factor), ~ na_if(as.character(.x), ""))) %>%
  dplyr::mutate(across(where(is.character), as.character)) %>%
  dplyr::mutate(across(where(is.factor), as.factor))

dat_q07_processed %<>%
  dplyr::mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  dplyr::mutate(across(where(is.factor), ~ na_if(as.character(.x), ""))) %>%
  dplyr::mutate(across(where(is.character), as.character)) %>%
  dplyr::mutate(across(where(is.factor), as.factor))

### Check Missing -----------------------------------------------------------------------------------------------------------

dat_q05_processed %>%
  summarise_all(~ sum(!is.na(.))) %>%
  select_if(~ . <= 10) %>%
  names()

dat_q06_processed %>%
  summarise_all(~ sum(!is.na(.))) %>%
  select_if(~ . <= 10) %>%
  names()

dat_q07_processed %>%
  summarise_all(~ sum(!is.na(.))) %>%
  select_if(~ . <= 10) %>%
  names()

### Combine Dataframes ------------------------------------------------------------------------------------------------------

# variables to retain for wave 5
varofinterest_q05 <- c(
  "subjID", "FamilyID", "ChildNr", "Sex_Child_Dummy01", "w05_sm_video", "w05_sm_gaming", "w05_sm_postandscroll", "w05_sm_messaging",
  "w05_sm_videocall", "w05_sm_total", "w05_PDS_1_C", "w05_PDS_2_C", "w05_PDS_3_C", "w05_PDS_Boys_4_C", "w05_PDS_Boys_5_C",
  "w05_PDS_Girls_4_C", "w05_PDS_Girls_5_C", "w05_PDS_total", "eatq_ec_total", "bisbas_total", "bisbas_bis", "bisbas_bas",
  "hscs_total", "sdq_total", "sdq_int", "sdq_ext", "cius_total"
)

# variables to retain for wave 6
varofinterest_q06 <- c(
  "subjID", "w06_sm_video", "w06_sm_gaming", "w06_sm_postandscroll", "w06_sm_messaging", "w06_sm_videocall", "w06_sm_total",
  "w06_PDS_1_Child", "w06_PDS_2_Child", "w06_PDS_3_Child", "w06_PDS_Boys_4_Chil", "w06_PDS_Boys_5_Chil", "w06_PDS_Girls_4_Chi",
  "w06_PDS_Girls_5_Chi", "w06_PDS_total", "eatq_ec_total", "bisbas_total", "bisbas_bis", "bisbas_bas", "hscs_total",
  "sdq_total", "sdq_int", "sdq_ext", "cius_total", "bsi_total", "bsi_depression", "bsi_anxiety", "bsi_sensitivity", "bsi_hostility", "bsi_wildman_check"
)

# variables to retain for wave 7
varofinterest_q07 <- c(
  "subjID", "Diagnosis_Q1", "Diagnosis_Q3", "Diagnosis_Q4", "Diagnosis_Q5", "w07_PDS_Q1", "w07_PDS_Q2", "w07_PDS_Q3", "w07_PDS_Boys_Q1",
  "w07_PDS_Boys_Q2", "w07_PDS_Girls_Q1", "w07_PDS_Girls_Q2", "w07_PDS_total", "w07_sm_video", "w07_sm_gaming",
  "w07_sm_postandscroll", "w07_sm_messaging", "w07_sm_videocall", "w07_sm_total", "eatq_ec_total", "bisbas_total", "bisbas_bis",
  "bisbas_bas", "hscs_total", "sdq_total", "sdq_int", "sdq_ext", "cius_total", "bsi_total", "bsi_depression", "bsi_anxiety", "bsi_sensitivity", "bsi_hostility",
  "bsi_wildman_check"
)

# remove variables from dataset
dat_q05_processed %<>%
  dplyr::select(all_of(varofinterest_q05)) %>%
  dplyr::rename(
    familyID = FamilyID,
    sex = Sex_Child_Dummy01, # Male = 0, Female = 1
    w05_pds_1 = w05_PDS_1_C,
    w05_pds_2 = w05_PDS_2_C,
    w05_pds_3 = w05_PDS_3_C,
    w05_pds_b_1 = w05_PDS_Boys_4_C,
    w05_pds_b_2 = w05_PDS_Boys_5_C,
    w05_pds_g_1 = w05_PDS_Girls_4_C,
    w05_pds_g_2 = w05_PDS_Girls_5_C,
    w05_pds_total = w05_PDS_total,
  ) %>%
  dplyr::rename_all(~ ifelse(. != "subjID" & !grepl("^w05_", .), paste0("w05_", .), .))

# remove variables from dataset
dat_q06_processed %<>%
  dplyr::select(all_of(varofinterest_q06)) %>%
  dplyr::rename(
    w06_pds_1 = w06_PDS_1_Child,
    w06_pds_2 = w06_PDS_2_Child,
    w06_pds_3 = w06_PDS_3_Child,
    w06_pds_b_1 = w06_PDS_Boys_4_Chil,
    w06_pds_b_2 = w06_PDS_Boys_5_Chil,
    w06_pds_g_1 = w06_PDS_Girls_4_Chi,
    w06_pds_g_2 = w06_PDS_Girls_5_Chi,
    w06_pds_total = w06_PDS_total,
  ) %>%
  dplyr::rename_all(~ ifelse(. != "subjID" & !grepl("^w06_", .), paste0("w06_", .), .))

# remove variables from dataset
dat_q07_processed %<>%
  dplyr::select(all_of(varofinterest_q07)) %>%
  dplyr::rename(
    w07_pds_1 = w07_PDS_Q1,
    w07_pds_2 = w07_PDS_Q2,
    w07_pds_3 = w07_PDS_Q3,
    w07_pds_b_1 = w07_PDS_Boys_Q1,
    w07_pds_b_2 = w07_PDS_Boys_Q2,
    w07_pds_g_1 = w07_PDS_Girls_Q1,
    w07_pds_g_2 = w07_PDS_Girls_Q2,
    w07_pds_total = w07_PDS_total,
    diagnosis_mh = Diagnosis_Q1,
    diagnosis_year = Diagnosis_Q3,
    diagnosis_current = Diagnosis_Q4,
    diagnosis_med = Diagnosis_Q5
  ) %>%
  dplyr::mutate(
    diagnosis_mh = ifelse(is.na(diagnosis_mh), NA, ifelse(diagnosis_mh == 1, TRUE, FALSE)),
    diagnosis_current = ifelse(is.na(diagnosis_current), NA, ifelse(diagnosis_current == 1, TRUE, FALSE)),
    diagnosis_med = ifelse(is.na(diagnosis_med), NA, ifelse(diagnosis_med == 1, TRUE, FALSE))
  ) %>%
  dplyr::rename_all(~ ifelse(. != "subjID" & !grepl("^w07_", .), paste0("w07_", .), .))

# merge questionnaire datasets
dat_main_processed <- dat_q05_processed %>%
  dplyr::left_join(dat_q06_processed, by = "subjID") %>%
  dplyr::left_join(dat_q07_processed, by = "subjID")

# merge questionnaire and demographic datasets
dat_merged_processed <- dat_q05_processed %>%
  dplyr::left_join(dat_q06_processed, by = "subjID") %>%
  dplyr::left_join(dat_q07_processed, by = "subjID") %>%
  dplyr::left_join(dat_demographics, by = "subjID")

## SAVE RESULTS =============================================================================================================

readr::write_csv(dat_q05_processed, file = here("data", "processed", "questionnaires_w05.csv"))
readr::write_csv(dat_q06_processed, file = here("data", "processed", "questionnaire_w06.csv"))
readr::write_csv(dat_q07_processed, file = here("data", "processed", "questionnaire_w07.csv"))
readr::write_csv(dat_main_processed, file = here("data", "processed", "questionnaire_all.csv"))
readr::write_csv(dat_merged_processed, file = here("data", "processed", "masterfile.csv"))

## ADD DEMOGRAPHICS =========================================================================================================

dat_merged_long <- dat_merged_processed %>%
  pivot_longer(
    cols = contains("w0"),
    names_to = c("wave", ".value"),
    names_pattern = "w(\\d+)_(.*)"
  ) %>%
  dplyr::select(subjID, wave, everything()) %>%
  mutate(wave = as.numeric(wave)) %>%
  arrange(subjID, wave) %>%
  select(subjID, wave, everything())

# generate descriptive statistics table for demographics
demographics_summary <- dat_merged_long %>%
  filter(wave == 1) %>%
  summarise(
    # Sex distribution
    Sex_Female = sum(sex_c == 1, na.rm = TRUE),
    Sex_Male = sum(sex_c == 0, na.rm = TRUE),
    Sex_Missing = sum(is.na(sex_c)),

    # age statistics
    Age_Mean = mean(age_c, na.rm = TRUE),
    Age_SD = sd(age_c, na.rm = TRUE),
    Age_Min = min(age_c, na.rm = TRUE),
    Age_Max = max(age_c, na.rm = TRUE),

    # handedness distribution
    Handedness_Right = sum(handedness_c == 1, na.rm = TRUE),
    Handedness_Left = sum(handedness_c == 2, na.rm = TRUE),
    Handedness_Ambidextrous = sum(handedness_c == 3, na.rm = TRUE),
    Handedness_Missing = sum(is.na(handedness_c)),

    # ethnicity distribution
    Ethnicity_Caucasian = sum(ethnicity_c == 1, na.rm = TRUE),
    Ethnicity_Other = sum(ethnicity_c != 1 & !is.na(ethnicity_c), na.rm = TRUE),
    Ethnicity_Missing = sum(is.na(ethnicity_c)),

    # SES distribution as categories
    SES_Low = sum(ses == 1, na.rm = TRUE),
    SES_Medium = sum(ses == 2, na.rm = TRUE),
    SES_High = sum(ses == 3, na.rm = TRUE),
    SES_Missing = sum(is.na(ses))
  )

# print the summary table
print(demographics_summary)

## VISUALISE DATA ===========================================================================================================

# generator vector with included participants at each wave
inclusion_vector <- c(
  sum(dat_demographics$w01_participation), sum(dat_demographics$w02_participation),
  sum(dat_demographics$w03_participation), sum(dat_demographics$w04_participation),
  sum(dat_demographics$w05_participation), sum(dat_demographics$w06_participation),
  sum(dat_demographics$w07_participation)
)

# create tibble for visualising sample size
inclusion_lcid <- tibble(dataset = rep("lcid", 7), as.numeric(c(1:7)), inclusion_vector) %>%
  dplyr::rename(wave = 2, subjects = 3)

# identity barchart for inclusions at each wave
lcid_plot <- inclusion_lcid %>%
  ggplot(., aes(fill = wave, y = subjects, x = wave)) +
  geom_bar(position = "dodge", stat = "identity", width = .70, show.legend = FALSE) +
  geom_text(aes(label = subjects),
    position = position_dodge(width = 1),
    vjust = -.3, size = 3.5, colour = "#5D5D5D", family = "sans", fontface = "italic"
  ) +
  scale_x_continuous(breaks = c(1:7)) +
  plot_theme +
  aspect_ratio_balanced +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, 600),
    breaks = seq(0, 600, 100)
  ) +
  theme(
    text = element_text(size = 12, family = "sans"),
    axis.text = element_text(size = 12, family = "sans")
  ) +
  labs(y = "Participants", x = "Wave") +
  scale_fill_viridis(option = "viridis", direction = 1, begin = .05, end = .95) +
  theme(axis.ticks.x = element_blank()) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = 600)

# save plot to working directory
ggplot2::ggsave(here("output", "sample", "lcid_samplesize.png"), dpi = 1200)

