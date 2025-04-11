## ======================================================================================================================= ##
## Script:    DATA INSPECTION AND VISUALISATION
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Wed Jun 12 18:33:57 2024
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

library(renv)
renv::restore()

set.seed(777) # set seed for random processes

# load required packages
library(grid)
library(here)
library(corrr)
library(purrr)
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(misty)
library(ggpubr)
library(ggdist)
library(ggExtra)
library(viridis)
library(ggplot2)
library(ggpmisc)
library(stringr)
library(ggridges)
library(ggthemes)
library(magrittr)
library(lmerTest)
library(tidybayes)
library(ggeffects)
library(gridExtra)
library(effectsize)
library(ggcorrplot)
library(colorspace)
library(parameters)
library(viridisLite)

# plot settings
options(ggplot2.continuous.colour = "viridis")
options(ggplot2.continuous.fill = "viridis")
theme_set(theme_classic())

# custom functions loaded
source(here::here("code", "functions", "fun_plots.R"))
source(here::here("code", "functions", "fun_helper.R"))
source(here::here("code", "functions", "fun_load_model_results.R"))

### Load Data ---------------------------------------------------------------------------------------------------------------
# dd_dat             = contains behavioural task data (list)
# dd_w02 ... dd_w06   = contains behavioural task data for each wave (df)
# datq05 ... datq07   = contains questionnaire results (processed in lcid_dd_preprocessing.R) (df)
# dat_qdem.           = contains merged questionnaire and demographics data (df)
# ddtinvar_wide       = contains time-invariant master dataset (wide format) (df)
# ddtinvar_long       = contains time-invariant master dataset (long format) (df)
# ddtvar_wide         = contains time-variant master dataset (wide format) (df)
# ddtvar_long         = contains time-variant master dataset (long format) (df)
# mod_results         = contains dataframes of each measurement wave model parameters (list)

# demographics data and task data loaded
dat_demographics <- read_csv(here::here("data", "processed", "demographics.csv"))
dd_master_df <- dd_dat <- readRDS(here::here("data", "processed", "dd_task.Rds"))

# trial level choice data
dd_choice <- read_rds(file = here::here("data", "processed", "dd_choice_data.RDS"))

# separate task data into waves
dd_w02 <- dd_dat[[1]]
dd_w03 <- dd_dat[[2]]
dd_w04 <- dd_dat[[3]]
dd_w05 <- dd_dat[[4]]
dd_w06 <- dd_dat[[5]]
ddtask02 <- dd_dat[[1]]
ddtask03 <- dd_dat[[2]]
ddtask04 <- dd_dat[[3]]
ddtask05 <- dd_dat[[4]]
ddtask06 <- dd_dat[[5]]

# masterdata including model results
ddtvar_wide <- utils::read.csv(file = here::here("data", "processed", "tvar_masterdat_wide.csv"), header = TRUE)
ddtvar_long <- utils::read.csv(file = here::here("data", "processed", "tvar_masterdat_long.csv"), header = TRUE)

# add log transformations
df <- ddtvar_wide %>%
  dplyr::mutate(
    logk2 = -log(w02_estimate_k),
    logk2_hdi_lower = -log(w02_hdi_lower_k),
    logk2_hdi_upper = -log(w02_hdi_upper_k),
    logk3 = -log(w03_estimate_k),
    logk3_hdi_lower = -log(w03_hdi_lower_k),
    logk3_hdi_upper = -log(w03_hdi_upper_k),
    logk4 = -log(w04_estimate_k),
    logk4_hdi_lower = -log(w04_hdi_lower_k),
    logk4_hdi_upper = -log(w04_hdi_upper_k),
    logk5 = -log(w05_estimate_k),
    logk5_hdi_lower = -log(w05_hdi_lower_k),
    logk5_hdi_upper = -log(w05_hdi_upper_k),
    logk6 = -log(w06_estimate_k),
    logk6_hdi_lower = -log(w06_hdi_lower_k),
    logk6_hdi_upper = -log(w06_hdi_upper_k)
  )

# load modeling results
dd_hyperbo_check_02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_02.RDS"))
dd_hyperbo_loo_02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_loo_02.RDS"))
dd_hyperbo_params_02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_parameters_02.RDS"))
dd_hyperbo_check_03 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_03.RDS"))
dd_hyperbo_loo_03 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_loo_03.RDS"))
dd_hyperbo_params_03 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_parameters_03.RDS"))
dd_hyperbo_check_04 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_04.RDS"))
dd_hyperbo_loo_04 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_loo_04.RDS"))
dd_hyperbo_params_04 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_parameters_04.RDS"))
dd_hyperbo_check_05 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_05.RDS"))
dd_hyperbo_loo_05 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_loo_05.RDS"))
dd_hyperbo_params_05 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_parameters_05.RDS"))
dd_hyperbo_check_06 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_06.RDS"))
dd_hyperbo_loo_06 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_loo_06.RDS"))
dd_hyperbo_params_06 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_parameters_06.RDS"))

## DESCRIPTIVES =============================================================================================================

# get number of male and female participants per wave
dat_demographics %>%
  group_by(sex_c) %>%
  summarise(
    w01 = sum(w01_participation, na.rm = TRUE),
    w02 = sum(w02_participation, na.rm = TRUE),
    w03 = sum(w03_participation, na.rm = TRUE),
    w04 = sum(w04_participation, na.rm = TRUE),
    w05 = sum(w05_participation, na.rm = TRUE),
    w06 = sum(w06_participation, na.rm = TRUE),
    w07 = sum(w07_participation, na.rm = TRUE),
    total_participants = n()
  )

# get age for male and female participants per wave
p_age <- dat_demographics %>%
  group_by(sex_c) %>%
  summarise(
    w01 = mean(w01_age_c, na.rm = TRUE),
    w02 = mean(w02_age_c, na.rm = TRUE),
    w03 = mean(w03_age_c, na.rm = TRUE),
    w04 = mean(w04_age_c, na.rm = TRUE),
    w05 = mean(w05_age_c, na.rm = TRUE),
    w06 = mean(w06_age_c, na.rm = TRUE),
    w07 = mean(w07_age, na.rm = TRUE),
    total_participants = n()
  )

# print with more digits
print.data.frame(p_age, digits = 5); base::remove(p_age)

#### Behavioural inhibition subscale ----------------------------------------------------------------------------------------

p_bis <- ddtvar_long %>%
  group_by(sex_c, wave) %>%
  summarise(
    mean_bis = mean(bisbas_bis, na.rm = TRUE),
    sd_bis = sd(bisbas_bis, na.rm = TRUE),
    n = n()
  )

# print with more digits
print.data.frame(p_bis, digits = 5); base::remove(p_bis)

#### Behavioural activation subscale ----------------------------------------------------------------------------------------

p_bas <- ddtvar_long %>%
  group_by(sex_c, wave) %>%
  summarise(
    mean_bas = mean(bisbas_bas, na.rm = TRUE),
    sd_bas = sd(bisbas_bas, na.rm = TRUE),
    n = n()
  )

# print with more digits
print.data.frame(p_bas, digits = 5); base::remove(p_bas)

#### Strength and difficulties questionnaire --------------------------------------------------------------------------------

p_sdq <- ddtvar_long %>%
  group_by(sex_c, wave) %>%
  summarise(
    mean_sdq = mean(sdq_total, na.rm = TRUE),
    sd_sdq = sd(sdq_total, na.rm = TRUE),
    n = n()
  )

# print with more digits
print.data.frame(p_sdq, digits = 5); base::remove(p_sdq)

#### Highly sensitive child scale -------------------------------------------------------------------------------------------

p_hscs <- ddtvar_long %>%
  group_by(sex_c, wave) %>%
  summarise(
    mean_hscs = mean(hscs_total, na.rm = TRUE),
    sd_hscs = sd(hscs_total, na.rm = TRUE),
    n = n()
  )

# print with more digits
print.data.frame(p_hscs, digits = 5); base::remove(p_hscs)

#### Early adolescent temperament questionnaire effortful control subscale --------------------------------------------------

p_eatq <- ddtvar_long %>%
  group_by(sex_c, wave) %>%
  summarise(
    mean_eatq = mean(eatq_ec_total, na.rm = TRUE),
    sd_eatq = sd(eatq_ec_total, na.rm = TRUE),
    n = n()
  )

# print with more digits
print.data.frame(p_eatq, digits = 5); base::remove(p_eatq)

#### Social media use -------------------------------------------------------------------------------------------------------

p_sm <- ddtvar_long %>%
  group_by(sex_c, wave) %>%
  summarise(
    mean_sm = mean(sm_postandscroll, na.rm = TRUE),
    sd_sm = sd(sm_postandscroll, na.rm = TRUE),
    n = n()
  )

# print with more digits
print.data.frame(p_sm, digits = 5); base::remove(p_sm)

#### Compulsive internet use scale ------------------------------------------------------------------------------------------

p_cius <- ddtvar_long %>%
  group_by(sex_c, wave) %>%
  summarise(
    mean_cius = mean(cius_total, na.rm = TRUE),
    sd_cius = sd(cius_total, na.rm = TRUE),
    n = n()
  )

# print with more digits
print.data.frame(p_cius, digits = 5); base::remove(p_cius)

#### Delay discounting ------------------------------------------------------------------------------------------------------

p_logk <- ddtvar_long %>%
  group_by(sex_c, wave) %>%
  summarise(
    mean_logk = mean(logk, na.rm = TRUE),
    sd_logk = sd(logk, na.rm = TRUE),
    n = n()
  )

# print with more digits
print.data.frame(p_logk, digits = 4); base::remove(p_logk)

### Examine Missingness -----------------------------------------------------------------------------------------------------

ddtvar_wide %>% # Check variables of high missingness in Wave 7
  select(contains("w07")) %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(key = "column", value = "na_count") %>%
  arrange(desc(na_count))

ddtvar_wide %>% # Check variables of high missingness in Wave 6
  select(contains("w06")) %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(key = "column", value = "na_count") %>%
  arrange(desc(na_count))

ddtvar_wide %>% # Check variables of high missingness in Wave 5
  select(contains("w05")) %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(key = "column", value = "na_count") %>%
  arrange(desc(na_count))

### Examine Dropout ---------------------------------------------------------------------------------------------------------

dat_demographics %>%
  select("subjID", "dropout", "dropout_before", contains("participation"))

dat_participation <- dat_demographics %>%
  summarise(across(contains("participation"), ~ mean(. == 1), .names = "prop_{.col}")) %>%
  pivot_longer(cols = starts_with("prop"), names_to = "wave", values_to = "retention_proportion")

ggplot(dat_participation, aes(x = wave, y = retention_proportion, fill = wave)) +
  geom_bar(stat = "identity") +
  plot_theme +
  labs(x = "Wave", y = "Proportion of participants retained") +
  geom_text(aes(label = scales::percent(retention_proportion)), vjust = -0.5, size = 3.5, color = "black") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5", "6", "7")) +
  scale_fill_viridis_d() +
  theme(
    axis.ticks.length.x = unit(0, "cm"),
    axis.text = element_text(size = 12),
    text = element_text(size = 12)
  ) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# data complete in waves 5 and 6
dat_demographics %>% # data available for W05 and W06
  filter(w05_participation == 1 & w06_participation == 1) %>%
  nrow()

# data complete in waves 5, 6 and 7
dat_demographics %>%
  filter(w05_participation == 1 & w06_participation == 1 & w07_participation == 1) %>%
  nrow()

#### Trial number density ---------------------------------------------------------------------------------------------------

dd_trials_den <- lapply(dd_dat, function(x) ggdensity(x, "trial") + dens_theme)

dd_trials_all <- ggarrange(
  plotlist = dd_trials_den, nrow = 3, ncol = 2, label.x = .5,
  labels = c("Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6"),
  font.label = list(size = 10, font = "plain")
)

ggplot2::ggsave(dd_trials_all,
  path = here("output", "images", "descriptives"),
  filename = "trials_distribution.png", dpi = 1200, device = "png"
)

#### Choice density ---------------------------------------------------------------------------------------------------------

dd_choices_den <- lapply(dd_dat, function(x) ggdensity(x, "choice") + dens_theme + scale_x_continuous(breaks = c(0, 1)))
dd_choices_all <- ggarrange(
  plotlist = dd_choices_den, nrow = 2, ncol = 3, label.x = .25,
  labels = c("Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6"), font.label = list(size = 10, font = "plain")
)

ggplot2::ggsave(dd_choices_all,
  path = here("output", "images", "descriptives"),
  filename = "choices_distribution.png", dpi = 1200, device = "png"
)

#### Choice proportion by age -----------------------------------------------------------------------------------------------

dd_dat_df <- dd_dat %>%
  map(~ as_tibble(.)) %>%
  bind_rows(.id = "index") %>%
  mutate(wave = as.numeric(str_sub(index, 7, 7))) %>%
  select(subjID, wave, everything(), -index)

# compute responding proportions per participant per wave
dd_dat_df %<>%
  group_by(wave, subjID) %>%
  count(choice) %>%
  group_by(wave, subjID) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = choice, values_from = prop) %>%
  dplyr::rename(
    prop_sooner = `0`,
    prop_later = `1`
  )

dd_dat_df <- ddtvar_long %>%
  dplyr::select(subjID, wave, age, estimate_k, logk) %>%
  left_join(., dd_dat_df, by = c("subjID", "wave")) %>%
  select(subjID, age, estimate_k, logk, prop_sooner, prop_later) %>%
  pivot_longer(cols = c("prop_sooner", "prop_later"), names_to = "choice", values_to = "prop")

# Save rds for choice computations
saveRDS(dd_dat_df, file = here("data", "processed", "choice_data.RDS"))

# visualise in plot
gg_age_choice <- dd_dat_df %>%
  ggplot2::ggplot(., aes(x = age, y = prop, color = choice)) +
  geom_point(alpha = .5, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .1)) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_colour_viridis_d(begin = 0, end = .95, labels = c("Later", "Sooner")) +
  plot_theme_legend +
  aspect_ratio_balanced +
  labs(x = "Child age", y = "Proportion of Choice", color = "Choice") +
  scale_x_continuous(
    breaks = seq(8, 15, 1), expand = c(0.04, 0),
    limits = c(dd_dat_df %>% filter(!is.na(prop)) %>% select(age) %>% min(.), 15)
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0.04, 0)) +
  annotate(x = 8, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA_character_),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

# save plot
ggplot2::ggsave(gg_age_choice,
  path = here("output", "images"), filename = "age_choice_interaction.png", dpi = 1200, device = "png"
)

#### Social media ~ age -----------------------------------------------------------------------------------------------------

sm_ps_age <- lmerTest::lmer(sm_postandscroll ~ age + (1 | subjID), data = ddtvar_long)
std_coef <- effectsize::standardize_parameters(sm_ps_age, method = "refit")

# examine results and standardised coefficients
print(summary(sm_ps_age), digits = 5)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(sm_ps_age)

# get predicted social media use
pred <- ggeffects::ggpredict(sm_ps_age, terms = "age")
print(pred, digits = 4)

#### Compulsive Internet Use ~ age --------------------------------------------------------------------------------------

cius_ps_age <- lmerTest::lmer(cius_total ~ age + (1 | subjID), data = ddtvar_long)
std_coef_cius <- effectsize::standardize_parameters(cius_ps_age, method = "refit")

# examine results and standardised coefficients
print(summary(cius_ps_age), digits = 5)
print(std_coef_cius, digits = 3)

# get R square
MuMIn::r.squaredGLMM(cius_ps_age)

pred <- ggeffects::ggpredict(cius_ps_age, terms = "age")

## DELAY DISCOUNTING PARAMETERS =============================================================================================

wave.labs <- c("Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6")
names(wave.labs) <- c("2", "3", "4", "5", "6")

# examine distribution of estimated discounting rates
k_dens <- ddtvar_long %>%
  filter(!is.na(estimate_k)) %>%
  group_by(wave) %>%
  ggplot(., aes(x = estimate_k, fill = wave)) +
  geom_density(colour = "#2E2E2E") +
  plot_theme +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1), labels = drop_leading_zeros) +
  xlab("Delay discounting parameter `k`") +
  ylab("Density") +
  facet_grid(~wave, labeller = labeller(wave = wave.labs)) +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.spacing = unit(.25, "cm")
  ) +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = .75, geom = "segment")

ggplot2::ggsave(k_dens, path = here("output", "images", "modeling"), filename = "k_density.png", dpi = 1200, device = "png")

# examine distribution of estimated discounting rates log-transformed
logk_dens <- ddtvar_long %>%
  filter(!is.na(estimate_k)) %>%
  group_by(wave) %>%
  mutate(log_k = -log(estimate_k)) %>%
  ggplot(., aes(x = log_k, fill = wave)) +
  geom_density(colour = "#2E2E2E") +
  plot_theme +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), labels = drop_leading_zeros) +
  xlab("Delay discounting parameter `log(k)`") +
  ylab("Density") +
  facet_grid(~wave, labeller = labeller(wave = wave.labs)) +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.spacing = unit(.25, "cm")
  ) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = .75, geom = "segment")

ggplot2::ggsave(
  logk_dens,
  path = here("output", "images", "modeling"),
  filename = "logk_density.png", dpi = 1200, device = "png"
)

### Proportion of offers accepted ~ Age -------------------------------------------------------------------------------------

dd_merge <- readRDS(file = here("data", "processed", "dd_choice_data.RDS"))

lmer_choice_age <- lmerTest::lmer(prop ~ age + (1 | subjID), data = dd_merge %>% filter(choice == "prop_later"))

std_coef <- effectsize::standardize_parameters(lmer_choice_age, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_choice_age), digits = 8)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_choice_age)

# get predicted behaviour
pred <- ggeffects::ggpredict(lmer_choice_age, terms = "age")

### Delay discounting ~ Age -------------------------------------------------------------------------------------

lmer_logk_age <- lmerTest::lmer(logk ~ age + (1 | subjID), data = dd_merge)

std_coef <- effectsize::standardize_parameters(lmer_logk_age, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_logk_age), digits = 8)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_logk_age)

# get predicted behaviour
pred <- ggeffects::ggpredict(lmer_logk_age, terms = "age")


### Proportion of offers accepted ~ Delay discounting -----------------------------------------------------------------------

lmer_choice_logk <- lmerTest::lmer(prop ~ logk + (1 | subjID), data = dd_merge %>% filter(choice == "prop_later"))

std_coef <- effectsize::standardize_parameters(lmer_choice_logk, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_choice_logk), digits = 8)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_choice_logk)

# get predicted behaviour
pred <- ggeffects::ggpredict(lmer_choice_logk, terms = "age")

### Social Media Use ~ Delay Discounting ------------------------------------------------------------------------------------

lmer_logk_sm <- lmerTest::lmer(sm_postandscroll ~ logk + (1 | subjID), data = ddtvar_long)
std_coef <- effectsize::standardize_parameters(lmer_logk_sm, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_logk_sm), digits = 6)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_logk_sm)

# get predicted social media use
pred <- ggpredict(lmer_logk_sm, terms = "logk")

### Compulsive Internet Use ~ Delay discounting -----------------------------------------------------------------------------

lmer_logk_cius <- lmerTest::lmer(cius_total ~ logk + (1 | subjID), data = ddtvar_long)
std_coef <- effectsize::standardize_parameters(lmer_logk_cius, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_logk_cius), digits = 5)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_logk_cius)

pred <- ggpredict(lmer_logk_cius, terms = "logk")

### Discounting Across Waves ------------------------------------------------------------------------------------------------

para_02 <- dd_hyperbo_params_02$individual_params %>%
  pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper))

para_03 <- dd_hyperbo_params_03$individual_params %>%
  pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper))

para_04 <- dd_hyperbo_params_04$individual_params %>%
  pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper))

para_05 <- dd_hyperbo_params_05$individual_params %>%
  pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper))

para_06 <- dd_hyperbo_params_06$individual_params %>%
  pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper))

# combine parameter frames across waves
param_all <- bind_rows(para_02, para_03, para_04, para_05, para_06) %>%
  mutate(wave = c(
    rep(2, nrow(para_02)), rep(3, nrow(para_03)), rep(4, nrow(para_04)),
    rep(5, nrow(para_05)), rep(6, nrow(para_06))
  ))

# plot discounting rate across waves
plot_para_k_develop <- param_all %>%
  mutate(wave = as.factor(wave)) %>%
  ggplot(., aes(x = wave, y = estimate_k, fill = wave)) +
  geom_violin(width = 1.5, alpha = .75) +
  geom_boxplot(width = 0.1, alpha = .2, colour = "#2E2E2E") +
  scale_colour_viridis_d(begin = 1, end = .15, direction = -1, aesthetics = c("colour", "fill")) +
  plot_theme +
  aspect_ratio_balanced +
  expand_limits(y = 0, x = 0) +
  annotate(x = .4, xend = .4, y = 0, yend = 1, colour = "black", lwd = .75, geom = "segment") +
  annotate(y = -Inf, yend = -Inf, x = 1, xend = 5, colour = "black", lwd = 0.75, geom = "segment") +
  coord_cartesian(xlim = c(1, 6))

ggplot2::ggsave(plot_para_k_develop,
  path = here("output", "images", "modeling"),
  filename = "parameters_development.png", dpi = 1200, device = "png"
)

### Acceptance Probabilities ------------------------------------------------------------------------------------------------

dd_merge <- readRDS(file = here("data", "processed", "dd_choice_data.RDS"))

cp02 <- choice_plot(ddtask02)
ggplot2::ggsave(cp02, path = here("output", "images", "descriptives"), filename = "dd_choice02.png", dpi = 1200, device = "png")
cp03 <- choice_plot(ddtask03)
ggplot2::ggsave(cp03, path = here("output", "images", "descriptives"), filename = "dd_choice03.png", dpi = 1200, device = "png")
cp04 <- choice_plot(ddtask04)
ggplot2::ggsave(cp04, path = here("output", "images", "descriptives"), filename = "dd_choice04.png", dpi = 1200, device = "png")
cp05 <- choice_plot(ddtask05)
ggplot2::ggsave(cp05, path = here("output", "images", "descriptives"), filename = "dd_choice05.png", dpi = 1200, device = "png")
cp06 <- choice_plot(ddtask06)
ggplot2::ggsave(cp06, path = here("output", "images", "descriptives"), filename = "dd_choice06.png", dpi = 1200, device = "png")

choice_combined <- ggarrange(cp02 + rremove("ylab") + rremove("xlab"),
  cp03 + rremove("ylab") + rremove("xlab"),
  cp04 + rremove("ylab") + rremove("xlab"),
  cp05 + rremove("ylab") + rremove("xlab"),
  cp06 + rremove("ylab") + rremove("xlab"),
  labels = NULL, common.legend = TRUE, ncol = 3, nrow = 2,
  align = "hv", legend = "right", label.x = "c", label.y = .5
)

choice_combined <- annotate_figure(choice_combined,
  left = grid::textGrob("Proportion of later offers selected (%)",
    rot = 90, vjust = 1, gp = gpar(cex = 1),
  ),
  bottom = grid::textGrob("Amount later - amount sooner", gp = gpar(cex = 1))
)

ggplot2::ggsave(choice_combined,
  path = here("output", "images", "descriptives"),
  filename = "dd_choice_combined.png", dpi = 1200, device = "png"
)

## CORRELATIONS =============================================================================================================

# select numeric variables
dd_cor <- ddtvar_wide %>%
  select(
    contains("logk"),
    contains("beta"),
    -contains("hdi"),
    contains("pds_total"),
    contains("sm_total"),
    contains("bisbas"),
    contains("sdq_total"),
    contains("hscs_total"),
    contains("eatq_ec_total"),
    contains("age"),
    contains("diagnosis"),
    contains("sm_total"),
    contains("education"),
    contains("ethnicity_c"),
    contains("bsi")
  ) %>%
  # columns with high missingness are removed as these are not suitable to be used as ancillary variables for FIML
  remove_high_na_columns(., threshold = 0.85) %>% # arbitrary decision of 85% threshold
  select(where(is.numeric))

dd_cor_mat <- cor(dd_cor, use = "pairwise.complete.obs")

# change format to long version
dd_cor_mat_long <- as.data.frame(as.table(dd_cor_mat))

# aename the columns for clarity
colnames(dd_cor_mat_long) <- c("Var1", "Var2", "correlation")

# remove duplicates and self-correlations (upper triangle)
dd_cor_mat_long %<>%
  filter(Var1 != Var2) %>% # remove self-correlations
  mutate(abs_correlation = abs(correlation)) %>% # add a column for the absolute value of the correlations
  arrange(desc(abs_correlation)) # sort by absolute value of correlation

#### Correlates of logk -----------------------------------------------------------------------------------------------------

# wave 5
dd_cor_mat_long %>%
  filter(Var1 == "w05_logk") %>%
  arrange(desc(abs_correlation)) %>%
  head(n = 20)

# wave 6
dd_cor_mat_long %>%
  filter(Var1 == "w06_logk") %>%
  arrange(desc(abs_correlation)) %>%
  head(n = 20)

#### Correlates of beta -----------------------------------------------------------------------------------------------------

# wave 5
dd_cor_mat_long %>%
  filter(Var1 == "w05_estimate_beta") %>%
  arrange(desc(abs_correlation)) %>%
  head(n = 20)

# wave 6
dd_cor_mat_long %>%
  filter(Var1 == "w06_estimate_beta") %>%
  arrange(desc(abs_correlation)) %>%
  head(n = 20)

## VISUALISATIONS ===========================================================================================================

### PLOTS: Social Media -----------------------------------------------------------------------------------------------------
# 0 == none
# 1 <= 1h
# 2 == 1 - 2h
# 3 == 2 - 3h
# 4 == 3 - 4h
# 5 >= 4h
# total social media = watching videos + messaging + videocall + scrolling and posting

#### PLOT: Social media use proportions -------------------------------------------------------------------------------------

lcid_sm_prop_bar <- ddtvar_long %>%
  # Format data for plotting
  dplyr::select(subjID, wave, contains("sm_"), -sm_total) %>%
  tidyr::pivot_longer(cols = contains("sm_"), names_to = "sm_type", values_to = "sm_score") %>%
  dplyr::group_by(wave) %>%
  dplyr::mutate(total_sm_score = sum(sm_score)) %>%
  dplyr::arrange(wave, desc(sm_score)) %>%
  dplyr::filter(sm_type != "sm_gaming") %>%
  dplyr::mutate(sm_type = factor(sm_type, levels = unique(sm_type))) %>%
  dplyr::ungroup() %>%
  # plot initialise
  ggplot2::ggplot(., aes(fill = sm_type, y = sm_score, x = wave)) +
  geom_bar(position = "fill", stat = "identity") +

  # plot formatting
  scale_y_continuous(breaks = seq(0, 1, .2), expand = c(0.04, 0)) +
  scale_fill_viridis_d(
    direction = -1, end = .95,
    labels = c("Messaging", "Posting and scrolling", "Watching videos", "Video calling")
  ) +
  labs(x = "Wave", y = "Proportion of time spent", fill = "Use type") +
  plot_theme_legend +
  aspect_ratio_narrow +
  annotate(x = 5, xend = 7, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ggplot2::ggsave(lcid_sm_prop_bar,
  path = here::here("output", "images", "descriptives"),
  filename = "sm_bar.png", dpi = 1200, device = "png"
)

#### PLOT: Social media stacked bar chart -----------------------------------------------------------------------------------

lcid_sm_bar_usetype <- ddtvar_long %>%
  select(-sm_total, -sm_gaming) %>%
  pivot_longer(cols = contains("sm_"), names_to = "sm_type", values_to = "sm_score") %>%
  select(subjID, wave, sm_type, sm_score) %>%
  group_by(wave, sm_type) %>%
  summarize(avg_sm_score = mean(sm_score, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = wave, y = avg_sm_score, fill = sm_type)) +
  geom_bar(stat = "identity", position = "stack") +
  plot_theme_legend +
  aspect_ratio_narrow +
  scale_fill_viridis_d(labels = c("Messaging", "Posting and scrolling", "Watching videos", "Video calling")) +
  labs(y = "Average hours spent", x = "Wave", fill = "Type of use") +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 7, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 5, xend = 7, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ggplot2::ggsave(lcid_sm_bar_usetype,
  path = here::here("output", "images", "descriptives"),
  filename = "sm_bar_usetype.png", dpi = 1200, device = "png"
)

#### PLOT: Social media stacked bar chart broken down by age ----------------------------------------------------------------

lcid_sm_bar_usetype_age <- ddtvar_long %>%
  select(-sm_total, -sm_gaming) %>%
  pivot_longer(cols = contains("sm_"), names_to = "sm_type", values_to = "sm_score") %>%
  mutate(age_rounded = round(age)) %>%
  mutate(age_rounded = as.factor(age_rounded)) %>%
  group_by(age_rounded, sm_type) %>%
  summarize(avg_sm_score = mean(sm_score, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(age_rounded) & !is.nan(avg_sm_score)) %>%
  ggplot(aes(x = age_rounded, y = avg_sm_score, fill = sm_type)) +
  geom_bar(stat = "identity", position = "stack") +
  plot_theme_legend +
  aspect_ratio_narrow +
  scale_fill_viridis_d() +
  scale_x_discrete(breaks = seq(11, 17, by = 1), expand = c(0.125, 0)) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1), expand = c(0.025, 0)) +
  labs(x = "Age (Rounded)", y = "Average Social Media Score") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 7, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 1, xend = 7, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ggplot2::ggsave(lcid_sm_bar_usetype_age,
  path = here::here("output", "images", "descriptives"),
  filename = "sm_bar_usetype_age.png", dpi = 1200, device = "png"
)

#### PLOT: Total time spent on social media across waves --------------------------------------------------------------------

lcid_sm_tot_den <- ddtvar_long %>%
  # format data for plotting
  dplyr::group_by(wave) %>%
  dplyr::filter(!is.na(sm_total)) %>%
  # plot initialise
  ggplot(aes(x = as.factor(wave), y = sm_total)) +
  ggdist::stat_halfeye(aes(color = wave, fill = after_scale(colorspace::lighten(color, 0))),
    adjust = 1, width = .75, .width = 0, justification = -.25, point_color = NA
  ) +
  geom_boxplot(aes(color = wave),
    width = .25, outlier.shape = NA
  ) +
  geom_point(aes(color = wave),
    alpha = .5, shape = 21, stroke = .4, size = 1.5,
    position = position_jitter(seed = 1, width = .12)
  ) +
  stat_summary(
    geom = "text", fun.data = add_sample,
    aes(
      label = paste("n =", ..label..), y = stage(sm_total, after_stat = 15), color = wave,
      color = after_scale(darken(color, .1, space = "HLS"))
    ), family = "Arial", size = 4, hjust = 0
  ) +

  # plot formatting
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  scale_colour_viridis_c(direction = 1, end = .7) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5), expand = c(0, 0)) +
  theme(axis.title.y = element_text(hjust = .40)) + # center y-axis title
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Wave", y = "Social media use") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 20, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment")

ggplot2::ggsave(lcid_sm_tot_den,
  path = here::here("output", "images"),
  filename = "sm_density.png", dpi = 1200, device = "png"
)

#### PLOT: Social media post and scroll across age --------------------------------------------------------------------------

# set labels for likert responses
likert_labels <- c("5" = ">4h",
                   "4" = "3-4h",
                   "3" = "2-3h",
                   "2" = "1-2h",
                   "1" = "0-1h",
                   "0" = "0h")

# set colour scheme for plot
viridis_colors <- viridis::viridis(6, option = "C", begin = .1, end = .8, direction = -1)
names(viridis_colors) <- likert_labels

# create the main plot
main_plot <- ddtvar_long %>%
  filter(!is.na(sm_postandscroll), !is.na(age)) %>%
  mutate(
    age_group = factor(floor(age)),
    sm_postandscroll = fct_rev(factor(sm_postandscroll,
                                      levels = c(0, 1, 2, 3, 4, 5),
                                      labels = names(viridis_colors)
    ))
  ) %>%
  count(age_group, sm_postandscroll) %>%
  group_by(age_group) %>%
  mutate(
    prop = n / sum(n),
    # Only create labels for proportions >= 0.05 (5%)
    label = ifelse(prop >= 0.05, scales::percent(prop, accuracy = 1), "")
  ) %>%
  arrange(age_group, desc(sm_postandscroll)) %>%
  mutate(
    # Calculate cumulative proportion for position
    cumulative = cumsum(prop) - prop,
    # Position text at the beginning of each bar with a small buffer
    position = cumulative + 0.01  # Small buffer from left edge
  ) %>%
  ungroup() %>%
  ggplot(aes(x = age_group, y = prop, fill = sm_postandscroll)) +
  geom_col(width = 0.8, color = "white", linewidth = 0) +
  geom_text(aes(y = position, label = label),
            color = "white",
            family = "Arial",  # Set font to Arial
            size = 10/.pt,     # Convert 10pt to ggplot2 size units
            hjust = 0,         # Left alignment
            vjust = 0.5) +     # Vertically centered
  coord_flip() +
  scale_fill_manual(values = viridis_colors) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  labs(
    x = "Age (years)",
    y = "Percentage of Responses"
  ) +
  theme_minimal() +
  plot_theme +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0, 0.75, 0.25, 0.25, "cm"),
    axis.title = element_text(face = "bold"),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    text = element_text(family = "Arial")  # Set global font to Arial
  ) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.5, geom = "segment")

# create colored text legend using same viridis colors
legend_plot <- ggplot() +
  annotate("text",
           x = seq(0.5, 6.5, length.out = 6),
           y = 1,
           label = rev(names(viridis_colors)),
           color = viridis_colors,
           family = "Arial",         # Added Arial font family
           size = 12/.pt,            # Convert 12pt to ggplot2 size units
           fontface = "bold") +
  xlim(0.5, 6.5) +
  theme_void() +
  theme(plot.margin = margin(0, 0.75, 0, 1, "cm"))

# Combine plots with proper spacing
sm_stacked_bar <- grid.arrange(
  legend_plot,
  main_plot,
  heights = c(0.05, .95),
  ncol = 1
)

ggplot2::ggsave(sm_stacked_bar,
                path = here::here("output", "images"),
                filename = "sm_stacked_bars.png",  width = 8, height = 5, dpi = 600, device = "png"
)


#### PLOT: Social media post and scroll across waves ------------------------------------------------------------------------

# create the main plot
main_plot_wave <- ddtvar_long %>%
  filter(!is.na(sm_postandscroll), !is.na(wave)) %>%
  mutate(
    wave = factor(wave),
    sm_postandscroll = fct_rev(factor(sm_postandscroll,
                                      levels = c(0, 1, 2, 3, 4, 5),
                                      labels = names(viridis_colors)
    ))
  ) %>%
  count(wave, sm_postandscroll) %>%
  group_by(wave) %>%
  mutate(
    prop = n / sum(n),
    # Only create labels for proportions >= 0.05 (5%)
    label = ifelse(prop >= 0.05, scales::percent(prop, accuracy = 1), "")
  ) %>%
  arrange(wave, desc(sm_postandscroll)) %>%
  mutate(
    # Calculate cumulative proportion for position
    cumulative = cumsum(prop) - prop,
    # Position text at the beginning of each bar with a small buffer
    position = cumulative + 0.01  # Small buffer from left edge
  ) %>%
  ungroup() %>%
  ggplot(aes(x = wave, y = prop, fill = sm_postandscroll)) +
  geom_col(width = 0.8, color = "white", linewidth = 0) +
  geom_text(aes(y = position, label = label),
            color = "white",
            family = "Arial",  # Set font to Arial
            size = 10/.pt,     # Convert 10pt to ggplot2 size units
            hjust = 0,         # Left alignment
            vjust = 0.5) +     # Vertically centered
  coord_flip() +
  scale_fill_manual(values = viridis_colors) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  labs(
    x = "Wave",
    y = "Percentage of Responses"
  ) +
  theme_minimal() +
  plot_theme +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0, 0.75, 0.25, 0.25, "cm"),
    axis.title = element_text(face = "bold"),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    text = element_text(family = "Arial")  # Set global font to Arial
  ) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.5, geom = "segment")

# Combine plots with proper spacing
sm_stacked_bar_wave <- grid.arrange(
  legend_plot,
  main_plot_wave,
  heights = c(0.05, .95),
  ncol = 1
)

ggplot2::ggsave(sm_stacked_bar_wave,
                path = here::here("output", "images"),
                filename = "sm_stacked_bars_wave.png",  width = 8, height = 5, dpi = 600, device = "png"
)

#### PLOT: Social media video watching across waves -------------------------------------------------------------------------

# create the main plot
sm_vid <- ddtvar_long %>%
  filter(!is.na(sm_video), !is.na(wave)) %>%
  mutate(
    wave = factor(wave),
    sm_video = fct_rev(factor(sm_video,
                                      levels = c(0, 1, 2, 3, 4, 5),
                                      labels = names(viridis_colors)
    ))
  ) %>%
  count(wave, sm_video) %>%
  group_by(wave) %>%
  mutate(
    prop = n / sum(n),
    # Only create labels for proportions >= 0.05 (5%)
    label = ifelse(prop >= 0.05, scales::percent(prop, accuracy = 1), "")
  ) %>%
  arrange(wave, desc(sm_video)) %>%
  mutate(
    # Calculate cumulative proportion for position
    cumulative = cumsum(prop) - prop,
    # Position text at the beginning of each bar with a small buffer
    position = cumulative + 0.01  # Small buffer from left edge
  ) %>%
  ungroup() %>%
  ggplot(aes(x = wave, y = prop, fill = sm_video)) +
  geom_col(width = 0.8, color = "white", linewidth = 0) +
  geom_text(aes(y = position, label = label),
            color = "white",
            family = "Arial",  # Set font to Arial
            size = 10/.pt,     # Convert 10pt to ggplot2 size units
            hjust = 0,         # Left alignment
            vjust = 0.5) +     # Vertically centered
  coord_flip() +
  scale_fill_manual(values = viridis_colors) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  labs(
    x = "Wave",
    y = "Percentage of Responses"
  ) +
  theme_minimal() +
  plot_theme +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0, 0.75, 0.25, 0.25, "cm"),
    axis.title = element_text(face = "bold"),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    text = element_text(family = "Arial")  # Set global font to Arial
  ) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.5, geom = "segment")

# Combine plots with proper spacing
sm_vid_stacked <- grid.arrange(
  legend_plot,
  sm_vid,
  heights = c(0.05, .95),
  ncol = 1
)

ggplot2::ggsave(sm_vid_stacked,
                path = here::here("output", "images", "descriptives"),
                filename = "sm_video_stacked.png",  width = 8, height = 5, dpi = 600, device = "png"
)

#### PLOT: Social media messaging across waves ------------------------------------------------------------------------------

# create the main plot
sm_vid <- ddtvar_long %>%
  filter(!is.na(sm_messaging), !is.na(wave)) %>%
  mutate(
    wave = factor(wave),
    sm_messaging = fct_rev(factor(sm_messaging,
                              levels = c(0, 1, 2, 3, 4, 5),
                              labels = names(viridis_colors)
    ))
  ) %>%
  count(wave, sm_messaging) %>%
  group_by(wave) %>%
  mutate(
    prop = n / sum(n),
    # Only create labels for proportions >= 0.05 (5%)
    label = ifelse(prop >= 0.05, scales::percent(prop, accuracy = 1), "")
  ) %>%
  arrange(wave, desc(sm_messaging)) %>%
  mutate(
    # Calculate cumulative proportion for position
    cumulative = cumsum(prop) - prop,
    # Position text at the beginning of each bar with a small buffer
    position = cumulative + 0.01  # Small buffer from left edge
  ) %>%
  ungroup() %>%
  ggplot(aes(x = wave, y = prop, fill = sm_messaging)) +
  geom_col(width = 0.8, color = "white", linewidth = 0) +
  geom_text(aes(y = position, label = label),
            color = "white",
            family = "Arial",  # Set font to Arial
            size = 10/.pt,     # Convert 10pt to ggplot2 size units
            hjust = 0,         # Left alignment
            vjust = 0.5) +     # Vertically centered
  coord_flip() +
  scale_fill_manual(values = viridis_colors) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  labs(
    x = "Wave",
    y = "Percentage of Responses"
  ) +
  theme_minimal() +
  plot_theme +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0, 0.75, 0.25, 0.25, "cm"),
    axis.title = element_text(face = "bold"),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    text = element_text(family = "Arial")  # Set global font to Arial
  ) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.5, geom = "segment")

# Combine plots with proper spacing
sm_messaging_stacked <- grid.arrange(
  legend_plot,
  sm_vid,
  heights = c(0.05, .95),
  ncol = 1
)

ggplot2::ggsave(sm_messaging_stacked,
                path = here::here("output", "images", "descriptives"),
                filename = "sm_messaging_stacked.png",  width = 8, height = 5, dpi = 600, device = "png"
)

#### PLOT: Social media videocalling across waves ---------------------------------------------------------------------------

# create the main plot
sm_vid <- ddtvar_long %>%
  filter(!is.na(sm_videocall), !is.na(wave)) %>%
  mutate(
    wave = factor(wave),
    sm_videocall = fct_rev(factor(sm_videocall,
                                  levels = c(0, 1, 2, 3, 4, 5),
                                  labels = names(viridis_colors)
    ))
  ) %>%
  count(wave, sm_videocall) %>%
  group_by(wave) %>%
  mutate(
    prop = n / sum(n),
    # Only create labels for proportions >= 0.05 (5%)
    label = ifelse(prop >= 0.05, scales::percent(prop, accuracy = 1), "")
  ) %>%
  arrange(wave, desc(sm_videocall)) %>%
  mutate(
    # Calculate cumulative proportion for position
    cumulative = cumsum(prop) - prop,
    # Position text at the beginning of each bar with a small buffer
    position = cumulative + 0.01  # Small buffer from left edge
  ) %>%
  ungroup() %>%
  ggplot(aes(x = wave, y = prop, fill = sm_videocall)) +
  geom_col(width = 0.8, color = "white", linewidth = 0) +
  geom_text(aes(y = position, label = label),
            color = "white",
            family = "Arial",  # Set font to Arial
            size = 10/.pt,     # Convert 10pt to ggplot2 size units
            hjust = 0,         # Left alignment
            vjust = 0.5) +     # Vertically centered
  coord_flip() +
  scale_fill_manual(values = viridis_colors) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  labs(
    x = "Wave",
    y = "Percentage of Responses"
  ) +
  theme_minimal() +
  plot_theme +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0, 0.75, 0.25, 0.25, "cm"),
    axis.title = element_text(face = "bold"),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    text = element_text(family = "Arial")  # Set global font to Arial
  ) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.5, geom = "segment")

# Combine plots with proper spacing
sm_videocall_stacked <- grid.arrange(
  legend_plot,
  sm_vid,
  heights = c(0.05, .95),
  ncol = 1
)

ggplot2::ggsave(sm_videocall_stacked,
                path = here::here("output", "images", "descriptives"),
                filename = "sm_videocall_stacked.png",  width = 8, height = 5, dpi = 600, device = "png"
)

### PLOTS: Compulsive Internet Use --------------------------------------------------------------------------------------

#### PLOT: Compulsive Internet Use across waves -------------------------------------------------------------------------

lcid_cius_tot_den <- ddtvar_long %>%
  # Format data for plotting
  dplyr::group_by(wave) %>%
  dplyr::filter(!is.na(cius_total)) %>%
  # Plot initialise
  ggplot(aes(x = as.factor(wave), y = cius_total)) +
  ggdist::stat_halfeye(aes(color = wave, fill = after_scale(lighten(color, 0))),
    adjust = 1, width = .75, .width = 0, justification = -.25, point_color = NA
  ) +
  geom_boxplot(aes(color = wave),
    width = .25, outlier.shape = NA
  ) +
  geom_point(aes(color = wave),
    alpha = .5, shape = 21, stroke = .4, size = 1.5,
    position = position_jitter(seed = 1, width = .12)
  ) +
  stat_summary(
    geom = "text", fun.data = add_sample,
    aes(
      label = paste("n =", ..label..), y = stage(sm_total, after_stat = 15), color = wave,
      color = after_scale(darken(color, .1, space = "HLS"))
    ), family = "Arial", size = 4, hjust = 0, vjust = 20
  ) +
  scale_colour_viridis_c() +
  scale_y_continuous(limits = c(10, 40), breaks = seq(10, 40, by = 5), expand = c(0, 0)) +
  theme(axis.title.x = element_text(hjust = .425)) + # center x-axis title
  plot_theme +
  aspect_ratio_narrow +
  labs(x = "Wave", y = "Compulsive SM use") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 2, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment")

ggplot2::ggsave(lcid_cius_tot_den,
  path = here::here("output", "images", "descriptives"),
  filename = "cius_density.png", dpi = 1200, device = "png"
)

#### PLOT: Compulsive Internet Use across age ---------------------------------------------------------------------------

dd_cius_age <- ddtvar_long %>%
  dplyr::filter(!is.na(cius_total)) %>%
  # Plot initialise
  ggplot(aes(x = age, y = cius_total)) +
  geom_point(aes(color = age),
    alpha = 1, shape = 16, stroke = .4, size = 1.5,
    position = position_jitter(seed = 1, width = .1)
  ) +
  geom_smooth(method = "lm", color = "#2E2E2E") +
  scale_colour_viridis_c(direction = -1) +
  scale_y_continuous(limits = c(10, 40), breaks = seq(10, 40, by = 5), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(11, 15.2), breaks = seq(11, 16, by = 1), expand = c(0.05, 0)) +
  theme(axis.title.x = element_text(hjust = 0.48)) + # center x-axis title
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Age", y = "Compulsive SM use") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = 1, geom = "segment") +
  annotate(x = 11, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 1, geom = "segment")

ggplot2::ggsave(dd_cius_age,
  path = here::here("output", "images", "descriptives"),
  filename = "cius_age.png", dpi = 1200, device = "png"
)

#### PLOT: Compulsive Internet Use and mental wellbeing indicator correlations ------------------------------------------

ddcors <- ddtvar_wide %>%
  select(
    contains("cius"),
    contains("bsi_total"),
    contains("sdq_total"),
    contains("hscs_total"),
    contains("bisbas_total"),
    contains("eatq_ec_total"),
    contains("hscs_total")
  ) %>%
  cor(., use = "pairwise.complete.obs")

ddcors %<>% round(., 2)

png("output/images/cius_correlation.png", width = 2000, height = 2000, res = 200, bg = NA)

corrplot::corrplot(ddcors,
  method = "color",
  type = "lower",
  order = "original",
  addCoef.col = "#2E2E2E",
  tl.col = "#2E2E2E",
  tl.srt = 45,
  diag = FALSE,
  mar = c(0, 0, 0, 0)
)

dev.off()

#### PLOT: Compulsive and 'regular' social media use ------------------------------------------------------------------------

ddtvar_long %>%
  ggplot(., aes(x = cius_total, y = sm_total)) +
  geom_jitter(colour = viridis(n = 1, alpha = .4, end = .8)) +
  geom_smooth(method = "lm", colour = "#2E2E2E", fill = "#A2AFB5", fullrange = TRUE, na.rm = TRUE) +
  plot_theme_legend +
  aspect_ratio_square +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 20, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 40, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  stat_poly_eq(method = "lm", label.x = .95, label.y = .95, use_label(c("eq"))) +
  labs(x = "Compulsive SM Use", y = "Total SM Use") +
  scale_colour_viridis(alpha = 1)

# remove rows with missing values
ddtvar_long_clean <- ddtvar_long %>%
  filter(!is.na(sm_postandscroll) & !is.na(cius_total))

# fit linear model and compute residuals
fit <- lm(cius_total ~ sm_postandscroll, data = ddtvar_long_clean)
ddtvar_long_clean <- ddtvar_long_clean %>%
  mutate(residuals = fit$residuals)

# plot with residual color coding
sm_cius_corr <- ddtvar_long_clean %>%
  ggplot(aes(x = sm_postandscroll, y = cius_total, color = abs(residuals))) +
  geom_jitter(width = .3, height = 0) +
  geom_smooth(method = "lm", colour = "#2E2E2E", fill = "#A2AFD9", fullrange = TRUE, na.rm = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_y_continuous(limits = c(10, 40), breaks = seq(10, 40, by = 5)) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  annotate("segment", x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = 0.75) +
  annotate("segment", x = 0, xend = 5, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
  stat_poly_eq(method = "lm", label.x = .95, label.y = .95, aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE) +
  labs(x = "Time spent (Posting and Scrolling)", y = "Compulsive internet use", colour = "Residual") +
  scale_colour_viridis(option = "D", alpha = 1)

sm_cius_corr_mar <- ggExtra::ggMarginal(sm_cius_corr, type = "density", size = 5, margins = "y", fill = "transparent")

#### PLOT: Compulsive and 'regular' social media use density plot -----------------------------------------------------------

sm_cius_dens <- ddtvar_long %>%
  mutate(sm_postandscroll_group = factor(sm_postandscroll)) %>%
  filter(!is.na(sm_postandscroll)) %>%
  ggplot(aes(x = cius_total, y = sm_postandscroll_group, fill = sm_postandscroll_group)) +
  geom_density_ridges(alpha = 1, scale = 0.7, rel_min_height = 0.001) +
  geom_boxplot(width = 0.2, alpha = 0.5, position = position_nudge(y = -0.1)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)) +
  scale_fill_viridis_d() +
  plot_theme +
  aspect_ratio_balanced +
  labs(
    x = "Compulsive Internet Use",
    y = "Time spent",
    fill = "Time spent"
  ) +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 0, xend = 40, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment")

ggplot2::ggsave(sm_cius_dens,
  path = here::here("output", "images", "descriptives"),
  filename = "sm_cius_dens.png", dpi = 1200, device = "png"
)
ggplot2::ggsave(sm_cius_corr_mar,
  path = here::here("output", "images", "descriptives"),
  filename = "sm_cius_corr_mar.png", dpi = 1200, device = "png"
)
ggplot2::ggsave(sm_cius_corr,
  path = here::here("output", "images", "descriptives"),
  filename = "sm_cius_corr.png", dpi = 1200, device = "png"
)

## WELLBEING ================================================================================================================

### BISBAS Scale ------------------------------------------------------------------------------------------------------------

#### BISBAS Total score -----------------------------------------------------------------------------------------------------

lcid_bisbas_den <- ddtvar_long %>%
  group_by(wave) %>%
  filter(!is.na(bisbas_total)) %>%
  ggplot(aes(x = as.factor(wave), y = bisbas_total)) +
  ggdist::stat_halfeye(
    aes(color = wave, fill = after_scale(lighten(color, 0))),
    adjust = 1, width = .75, .width = 0, justification = -.25, point_color = NA
  ) +
  geom_point(aes(color = wave),
             alpha = .5, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .12)
  ) +
  geom_boxplot(
    aes(
      color = wave, color = after_scale(darken(color, .8, space = "HLS")),
      fill = after_scale(desaturate(lighten(color, .3), .4)), alpha = .7
    ),
    width = .25, outlier.shape = NA
  ) +
  stat_summary(
    geom = "text", fun.data = add_sample,
    aes(
      label = paste("n =", ..label..), y = stage(bisbas_total, after_stat = 15), x = (wave - 4),
      color = wave, color = after_scale(darken(color, .1, space = "HLS"))
    ),
    family = "Arial", size = 4, hjust = 0
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  scale_fill_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_colour_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  labs(x = "Wave", y = "BISBAS score") +
  plot_theme +
  aspect_ratio_balanced +
  scale_y_continuous(limits = c(15, 85), breaks = seq(15, 85, by = 10), expand = c(.05, 0)) +
  annotate(x = -Inf, xend = -Inf, y = 15, yend = 85, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(
    axis.title.y = element_text(hjust = .41),
    text = element_text(size = 12 / .pt),
    axis.title = element_text(size = 12, face = "bold")
  )

ggplot2::ggsave(lcid_bisbas_den,
                path = here::here("output", "images", "descriptives"),
                filename = "bisbas_dens.png", dpi = 600, device = "png"
)

#### BISBAS BIS Subscale ----------------------------------------------------------------------------------------------------

lcid_bis_den <- ddtvar_long %>%
  group_by(wave) %>%
  filter(!is.na(bisbas_bis)) %>%
  ggplot(aes(x = as.factor(wave), y = bisbas_bis)) +
  ggdist::stat_halfeye(
    aes(
      color = wave,
      fill = after_scale(lighten(color, 0))
    ),
    adjust = 1,
    width = .75,
    .width = 0,
    justification = -.25,
    point_color = NA
  ) +
  geom_point(
    aes(color = wave),
    alpha = .5, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .12)
  ) +
  geom_boxplot(
    aes(
      color = wave, color = after_scale(darken(color, .8, space = "HLS")),
      fill = after_scale(desaturate(lighten(color, .3), .4)), alpha = .7
    ),
    width = .25,
    outlier.shape = NA
  ) +
  stat_summary(
    geom = "text",
    fun.data = add_sample,
    aes(
      label = paste("n =", ..label..), y = 0, x = (wave - 4),
      color = wave, color = after_scale(darken(color, .1, space = "HLS"))
    ),
    family = "Arial", size = 4, hjust = 0
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  scale_fill_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_colour_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  labs(x = "Wave", y = "BIS score") +
  plot_theme +
  aspect_ratio_balanced +
  scale_y_continuous(limits = c(5, 35), breaks = seq(5, 35, by = 5), expand = c(.05, 0)) +
  annotate(x = -Inf, xend = -Inf, y = 5, yend = 35, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(
    axis.title.y = element_text(hjust = .41),
    axis.title = element_text(size = 12, face = "bold")
  )

ggplot2::ggsave(lcid_bis_den,
                path = here::here("output", "images", "descriptives"),
                filename = "bis_dens.png", dpi = 600, device = "png"
)

#### BISBAS BAS Subscale score ----------------------------------------------------------------------------------------------

lcid_bas_den <- ddtvar_long %>%
  group_by(wave) %>%
  filter(!is.na(bisbas_bas)) %>%
  ggplot(aes(x = as.factor(wave), y = bisbas_bas)) +
  ggdist::stat_halfeye(
    aes(
      color = wave,
      fill = after_scale(lighten(color, 0))
    ),
    adjust = 1,
    width = .75,
    .width = 0,
    justification = -.25,
    point_color = NA
  ) +
  geom_point(
    aes(color = wave),
    alpha = .5, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .12)
  ) +
  geom_boxplot(
    aes(
      color = wave, color = after_scale(darken(color, .8, space = "HLS")),
      fill = after_scale(desaturate(lighten(color, .3), .4))
    ),
    width = .25,
    outlier.shape = NA,
    alpha = .7
  ) +
  stat_summary(
    geom = "text",
    fun.data = add_sample,
    aes(
      label = paste("n =", ..label..), y = 0, x = (wave - 4),
      color = wave, color = after_scale(darken(color, .1, space = "HLS"))
    ),
    family = "Arial", size = 4, hjust = 0
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  labs(x = "Wave", y = "BAS score") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_colour_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_y_continuous(limits = c(10, 60), breaks = seq(10, 60, by = 10), expand = c(.05, 0)) +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 60, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(
    axis.title.y = element_text(hjust = .41),
    axis.title = element_text(size = 12, face = "bold")
  )

ggplot2::ggsave(lcid_bas_den,
                path = here::here("output", "images", "descriptives"),
                filename = "bas_dens.png", dpi = 1200, device = "png"
)

# merge above bisbas plots on single output
bisbas_dens <- ggpubr::ggarrange(
  lcid_bisbas_den,
  lcid_bis_den,
  lcid_bas_den,
  heights = c(1, 1, 1),
  widths = c(1, 1, 1),
  ncol = 3,
  labels = c("A", "B", "C"), label.y = .8,
  align = "v"
)

ggplot2::ggsave(bisbas_dens,
                path = here::here("output", "images", "descriptives"),
                filename = "bisbas_dens.png", dpi = 600, device = "png",
                height = 5, width = 10
)

### SDQ Scale ---------------------------------------------------------------------------------------------------------------

#### SDQ Scale total score --------------------------------------------------------------------------------------------------

lcid_sdq_den <- ddtvar_long %>%
  group_by(wave) %>%
  filter(!is.na(sdq_total)) %>%
  ggplot(aes(x = as.factor(wave), y = sdq_total)) +
  ggdist::stat_halfeye(
    aes(
      color = wave,
      fill = after_scale(lighten(color, 0))
    ),
    adjust = 1,
    width = .75,
    .width = 0,
    justification = -.25,
    point_color = NA
  ) +
  geom_point(
    aes(color = wave),
    alpha = .5, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .12)
  ) +
  geom_boxplot(
    aes(
      color = wave, color = after_scale(darken(color, .8, space = "HLS")),
      fill = after_scale(desaturate(lighten(color, .3), .4)), alpha = .7
    ),
    width = .25,
    outlier.shape = NA
  ) +
  stat_summary(
    geom = "text",
    fun.data = add_sample,
    aes(
      label = paste("n =", ..label..), y = 0, x = (wave - 4),
      color = wave, color = after_scale(darken(color, .1, space = "HLS"))
    ),
    family = "Arial", size = 12/.pt, hjust = 0
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  scale_fill_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_colour_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  labs(x = "Wave", y = "SDQ score") +
  plot_theme +
  aspect_ratio_balanced +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10), expand = c(.05, 0)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 40, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(
    axis.title.y = element_text(hjust = .41),
    axis.title = element_text(size = 12, face = "bold")
  )

#### SDQ Scale internalising subscale ---------------------------------------------------------------------------------------

lcid_sdq_int_den <- ddtvar_long %>%
  group_by(wave) %>%
  filter(!is.na(sdq_int)) %>%
  ggplot(aes(x = as.factor(wave), y = sdq_int)) +
  ggdist::stat_halfeye(
    aes(
      color = wave,
      fill = after_scale(lighten(color, 0))
    ),
    adjust = 1,
    width = .75,
    .width = 0,
    justification = -.25,
    point_color = NA
  ) +
  geom_point(
    aes(color = wave),
    alpha = .5, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .12)
  ) +
  geom_boxplot(
    aes(
      color = wave, color = after_scale(darken(color, .8, space = "HLS")),
      fill = after_scale(desaturate(lighten(color, .3), .4)), alpha = .7
    ),
    width = .25,
    outlier.shape = NA
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  scale_fill_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_colour_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  labs(x = "Wave", y = "SDQ internalising score") +
  plot_theme +
  aspect_ratio_balanced +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5), expand = c(.05, 0)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 20, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(
    axis.title.y = element_text(hjust = .41),
    axis.title = element_text(size = 12, face = "bold")
  )

#### SDQ Scale externalising subscale ---------------------------------------------------------------------------------------

lcid_sdq_ext_den <- ddtvar_long %>%
  group_by(wave) %>%
  filter(!is.na(sdq_ext)) %>%
  ggplot(aes(x = as.factor(wave), y = sdq_ext)) +
  ggdist::stat_halfeye(
    aes(
      color = wave,
      fill = after_scale(lighten(color, 0))
    ),
    adjust = 1,
    width = .75,
    .width = 0,
    justification = -.25,
    point_color = NA
  ) +
  geom_point(
    aes(color = wave),
    alpha = .5, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .12)
  ) +
  geom_boxplot(
    aes(
      color = wave, color = after_scale(darken(color, .8, space = "HLS")),
      fill = after_scale(desaturate(lighten(color, .3), .4)), alpha = .7
    ),
    width = .25,
    outlier.shape = NA
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  scale_fill_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_colour_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  labs(x = "Wave", y = "SDQ externalising score") +
  plot_theme +
  aspect_ratio_balanced +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5), expand = c(.05, 0)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 20, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(
    axis.title.y = element_text(hjust = .41),
    axis.title = element_text(size = 12, face = "bold")
  )

# merge above bisbas plots on single output
sdq_dens <- ggpubr::ggarrange(
  lcid_sdq_den,
  lcid_sdq_int_den,
  lcid_sdq_ext_den,
  heights = c(1, 1, 1),
  widths = c(1, 1, 1),
  ncol = 3,
  labels = c("A", "B", "C"), label.y = .8,
  align = "v"
)

ggplot2::ggsave(sdq_dens,
                path = here::here("output", "images", "descriptives"),
                filename = "sdq_dens.png", dpi = 600, device = "png",
                height = 5, width = 10
)


### HSCS Scale --------------------------------------------------------------------------------------------------------------

#### HSCS Scale total score -------------------------------------------------------------------------------------------------

lcid_hscs_den <- ddtvar_long %>%
  group_by(wave) %>%
  filter(!is.na(hscs_total)) %>%
  ggplot(aes(x = as.factor(wave), y = hscs_total)) +
  ggdist::stat_halfeye(
    aes(
      color = wave,
      fill = after_scale(lighten(color, 0))
    ),
    adjust = 1,
    width = .75,
    .width = 0,
    justification = -.25,
    point_color = NA
  ) +
  geom_point(
    aes(color = wave),
    alpha = .5, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .12)
  ) +
  geom_boxplot(
    aes(
      color = wave, color = after_scale(darken(color, .8, space = "HLS")),
      fill = after_scale(desaturate(lighten(color, .3), .4)), alpha = .7
    ),
    width = .25,
    outlier.shape = NA
  ) +
  stat_summary(
    geom = "text",
    fun.data = add_sample,
    aes(
      label = paste("n =", ..label..), y = 0, x = (wave - 4),
      color = wave, color = after_scale(darken(color, .1, space = "HLS"))
    ),
    family = "Arial", size = 12/.pt, hjust = 0
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  scale_fill_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_colour_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  labs(x = "Wave", y = "HSCS score") +
  plot_theme +
  aspect_ratio_balanced +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10), expand = c(.05, 0)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 90, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(
    axis.title.y = element_text(hjust = .41),
    axis.title = element_text(size = 12, face = "bold")
  )

### EATQ Scale --------------------------------------------------------------------------------------------------------------

#### EATQ-EC subscale score -------------------------------------------------------------------------------------------------

lcid_eatq_den <- ddtvar_long %>%
  group_by(wave) %>%
  filter(!is.na(eatq_ec_total)) %>%
  ggplot(aes(x = as.factor(wave), y = eatq_ec_total)) +
  ggdist::stat_halfeye(
    aes(
      color = wave,
      fill = after_scale(lighten(color, 0))
    ),
    adjust = 1,
    width = .75,
    .width = 0,
    justification = -.25,
    point_color = NA
  ) +
  geom_point(
    aes(color = wave),
    alpha = .5, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .12)
  ) +
  geom_boxplot(
    aes(
      color = wave, color = after_scale(darken(color, .8, space = "HLS")),
      fill = after_scale(desaturate(lighten(color, .3), .4)), alpha = .7
    ),
    width = .25,
    outlier.shape = NA
  ) +
  stat_summary(
    geom = "text",
    fun.data = add_sample,
    aes(
      label = paste("n =", ..label..), y = 30, x = (wave - 4),
      color = wave, color = after_scale(darken(color, .1, space = "HLS"))
    ),
    family = "Arial", size = 12/.pt, hjust = 0
  ) +
  coord_flip(xlim = c(1.2, NA), clip = "off") +
  scale_fill_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  scale_colour_viridis_c(begin = .1, end = .8, option = "C", direction = -1) +
  labs(x = "Wave", y = "EATQ-EC score") +
  plot_theme +
  aspect_ratio_balanced +
  scale_y_continuous(limits = c(30, 70), breaks = seq(30, 70, by = 10), expand = c(.05, 0)) +
  annotate(x = -Inf, xend = -Inf, y = 30, yend = 70, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(
    axis.title.y = element_text(hjust = .41),
    axis.title = element_text(size = 12, face = "bold")
  )

# merge above bisbas plots on single output
hscs_eatq_dens <- ggpubr::ggarrange(
  lcid_hscs_den,
  lcid_eatq_den,
  widths = c(1, 1),
  heights = c(1, 1),
  ncol = 2,
  labels = c("A", "B"), label.y = .8,
  align = "v"
)

ggplot2::ggsave(hscs_eatq_dens,
                path = here::here("output", "images", "descriptives"),
                filename = "hscs_eatq_denss.png", dpi = 600, device = "png",
                height = 5, width = 10
)









































#### PLOT: Simulated visualisation for hyperbolic model (k)  ----------------------------------------------------------------

# create a new tibble with the desired rows
dd_model_sim <- tibble(
  delay = round(runif(10000, 0, 80)),
  reward = runif(10000, 100, 100),
  k = sample(c(0.01, 0.1, 1), 10000, replace = TRUE)
) %>%
  mutate(subjective_value = reward / (1 + k * delay))

dd_sval_demo <- dd_model_sim %>%
  mutate(k = as.factor(k)) %>%
  ggplot(aes(x = delay, y = subjective_value, colour = k, group = k)) +
  geom_point(aes(colour = k), size = .5) +
  geom_line(aes(colour = k)) +
  scale_colour_viridis_d(direction = -1, option = "viridis", begin = .3, end = .8, labels = c("k = 0.1", "k = 1", "k = 10")) +
  plot_theme_legend +
  aspect_ratio_balanced +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 100, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 0, xend = 80, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  theme(legend.position = c(.8, .8)) +
  labs(x = " Delay [Days]", y = "Subjective Value [£]", colour = "")

ggplot2::ggsave(dd_sval_demo,
  path = here::here("output", "images"),
  filename = "hyperbolic_simulation.png", dpi = 1200, device = "png"
)

#### PLOT: Simulated visualisation for hyperbolic model softmax (beta) ------------------------------------------------------

dd_soft_sim <- tibble(
  reward_sooner = round(runif(10000, 0, 100)),
  reward_delayed = rep(100, 10000),
  delay = sample(c(0, 10, 20, 30), replace = TRUE, size = 10000),
  k = sample(seq(0, 1, .01), 10000, replace = TRUE),
  beta = sample(c(0.1, .5, 1), 10000, replace = TRUE)
) %>%
  mutate(
    sv_delayed = reward_delayed / (1 + k * delay),
    sv_sooner = reward_sooner / (1 + k * 0),
    p_delayed = 1 / ((1 + exp(-beta * (sv_delayed - sv_sooner)))),
    sv_diff = sv_delayed - sv_sooner
  )

# Stan code
# ev_later   = amount_later[i, t]  / (1 + k[i] * delay_later[i, t]);
# ev_sooner  = amount_sooner[i, t] / (1 + k[i] * delay_sooner[i, t]);
# choice[i, t] ~ bernoulli_logit(beta[i] * (ev_later - ev_sooner));

gg_soft_sim <- dd_soft_sim %>%
  mutate(beta = as.factor(beta)) %>%
  ggplot(aes(x = sv_diff, y = p_delayed, colour = beta, group = beta)) +
  geom_line(aes(colour = beta), size = 1.25) +
  # geom_point(aes(colour = beta), size = .5) +
  plot_theme_legend +
  aspect_ratio_balanced +
  scale_x_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 5)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(aes(colour = beta)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = -15, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  scale_colour_viridis_d(begin = 0, end = 1, labels = c("β = 0.1", "β = 1", "β = 10")) +
  theme(legend.position = c(0.8, 0.3)) +
  labs(y = "Acceptance probability (delayed)", x = "Subjective value later - sooner", colour = "")

ggplot2::ggsave(gg_soft_sim,
  path = here::here("output", "images"),
  filename = "softmax_simulation.png", dpi = 1200, device = "png"
)

### PLOTS: Delay Discounting Descriptives -----------------------------------------------------------------------------------

dd_choice_logk <- dd_choice %>%
  ggplot(., aes(x = logk, y = prop, color = choice, fill = choice)) +
  geom_point(alpha = .3, aes(colour = choice), shape = 16, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, fullrange = TRUE) +
  plot_theme_legend +
  aspect_ratio_balanced +
  theme(element_text(size = 4)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3)) +
  scale_fill_viridis_d(end = 1, name = "Choice", labels = c("Proportion later", "Proportion sooner")) +
  scale_colour_viridis_d(end = 1, name = "Choice", labels = c("Proportion later", "Proportion sooner")) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.5, geom = "segment") +
  labs(x = "Delay discounting parameter (logk)", y = "Proportion of responses")

ggplot2::ggsave(dd_choice_logk,
  path = here::here("output", "images"),
  filename = "logk_choice.png", dpi = 1200, device = "png"
)

#### PLOT: Delay discounting moderation wave 5 ------------------------------------------------------------------------------

dd_w05_mod <- ddtvar_wide %>%
  filter(!is.na(w05_logk), !is.na(w05_sm_total), !is.na(w06_bisbas_total)) %>%
  mutate(quartile = as.factor(ntile(w05_logk, 4))) %>%
  ggplot(., aes(x = w05_sm_total, y = w06_bisbas_total, color = quartile)) +
  geom_point(alpha = .75, shape = 16, size = 1.5, position = position_jitter(seed = 1, width = .1)) +
  geom_smooth(method = "lm", formula = y ~ x, size = 1, span = 1) +
  plot_theme_legend +
  aspect_ratio_narrow +
  facet_wrap(. ~ quartile, scales = "free", ncol = 4) +
  theme(panel.spacing = unit(.5, "cm", data = NULL)) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5), expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(28, 72), breaks = seq(30, 70, 10), expand = c(0, 0)) +
  annotate(x = 0, xend = 20, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 30, yend = 70, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  scale_colour_viridis_d()

ggplot2::ggsave(dd_w05_mod,
  path = here::here("output", "images", "descriptives"),
  filename = "w05_mod.png", dpi = 1200, device = "png"
)

#### PLOT: Delay discounting moderation wave 6 ------------------------------------------------------------------------------

ddtvar_wide %>%
  filter(!is.na(w05_estimate_k), !is.na(w05_sm_total), !is.na(w07_bisbas_total)) %>%
  nrow(.)

dd_w06_mod <- ddtvar_wide %>%
  filter(!is.na(w06_logk), !is.na(w06_sm_total), !is.na(w07_bisbas_total)) %>%
  mutate(quartile = as.factor(ntile(w06_logk, 4))) %>%
  ggplot(., aes(x = w06_sm_total, y = w07_bisbas_total, color = quartile)) +
  geom_point(alpha = .75, shape = 16, size = 1.5, position = position_jitter(seed = 1, width = .1)) +
  geom_smooth(method = "lm", formula = y ~ x, size = 1, span = 1) +
  plot_theme_legend +
  aspect_ratio_narrow +
  facet_wrap(. ~ quartile, scales = "free", ncol = 4) +
  theme(panel.spacing = unit(.5, "cm", data = NULL)) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5), expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(28, 72), breaks = seq(30, 70, 10), expand = c(0, 0)) +
  annotate(x = 0, xend = 20, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 30, yend = 70, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  scale_colour_viridis_d()

ggplot2::ggsave(dd_w06_mod,
  path = here::here("output", "images", "descriptives"),
  filename = "w06_mod.png", dpi = 1200, device = "png"
)

#### PLOT: Delay discounting moderation of Compulsive Internet Use wave 6 -----------------------------------------------

dd_w06_mod_cius <- ddtvar_wide %>%
  filter(!is.na(w05_logk), !is.na(w05_cius_total), !is.na(w06_bisbas_total)) %>%
  mutate(quartile = as.factor(ntile(w05_logk, 4))) %>%
  ggplot(., aes(x = w05_cius_total, y = w06_bisbas_total, color = quartile)) +
  geom_point(alpha = .75, shape = 16, size = 1.5, position = position_jitter(seed = 1, width = .1)) +
  geom_smooth(method = "lm", formula = y ~ x, size = 1, span = 1) +
  plot_theme_legend +
  aspect_ratio_narrow +
  facet_wrap(. ~ quartile, scales = "free", ncol = 4) +
  theme(panel.spacing = unit(.5, "cm", data = NULL)) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5), expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(28, 72), breaks = seq(30, 70, 10), expand = c(0, 0)) +
  annotate(x = 0, xend = 20, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 30, yend = 70, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  scale_colour_viridis_d()

ggplot2::ggsave(dd_w06_mod_cius,
  path = here::here("output", "images", "descriptives"),
  filename = "w06_mod_cius.png", dpi = 1200, device = "png"
)

### PLOTS: Social Media Use and Wellbeing -----------------------------------------------------------------------------------

ddtvar_long %>%
  filter(!is.na(logk), !is.na(bsi_total)) %>%
  ggplot(., aes(x = logk, y = bsi_total)) +
  geom_point(alpha = .75, shape = 16, size = 1.5, position = position_jitter(seed = 1, width = .1), aes(colour = bsi_total)) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1, colour = "#2E2E2E") +
  plot_theme +
  aspect_ratio_balanced +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(20, 80), breaks = seq(20, 80, 10), expand = c(0, 0)) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 20, yend = 80, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  scale_colour_viridis(end = .85)

dd_logk_bis <- ddtvar_long %>%
  filter(!is.na(logk), !is.na(bisbas_bis)) %>%
  ggplot(., aes(x = logk, y = bisbas_bis)) +
  geom_point(alpha = .75, shape = 16, size = 1.5, position = position_jitter(seed = 1, width = .1), aes(colour = bsi_total)) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1, colour = "#2E2E2E") +
  plot_theme +
  aspect_ratio_balanced +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5), expand = c(0, 0)) +
  annotate(x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 30, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  scale_colour_viridis(end = .85)

dd_logk_bas <- ddtvar_long %>%
  filter(!is.na(logk), !is.na(bisbas_bas)) %>%
  ggplot(., aes(x = logk, y = bisbas_bas)) +
  geom_point(alpha = .75, shape = 16, size = 1.5, position = position_jitter(seed = 1, width = .1), aes(colour = bsi_total)) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1, colour = "#2E2E2E") +
  plot_theme +
  aspect_ratio_balanced +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(25, 55), breaks = seq(25, 55, 10), expand = c(0, 0)) +
  annotate(x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 25, yend = 55, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  scale_colour_viridis(end = .85)

ggplot2::ggsave(dd_logk_bis,
  path = here::here("output", "images", "descriptives"),
  filename = "dd_logk_bis.png", dpi = 1200, device = "png"
)

ggplot2::ggsave(dd_logk_bas,
  path = here::here("output", "images", "descriptives"),
  filename = "dd_logk_bas.png", dpi = 1200, device = "png"
)


cor(ddtvar_long$logk, ddtvar_long$bsi_total, use = "pairwise.complete.obs")

#### PLOT: Delay discounting correlation matrix -----------------------------------------------------------------------------

cors <- ddtvar_long %>%
  select(
    subjID, logk, bsi_total, bsi_anxiety, bsi_depression, bisbas_total, bisbas_bis, bisbas_bas,
    sdq_total, hscs_total, eatq_ec_total
  ) %>%
  corrr::correlate(., method = "pearson", use = "pairwise.complete.obs") %>%
  corrr::rearrange(.)

corplots <- cors %>%
  corrr::rplot(., print_cor = .05) +
  aspect_ratio_wide +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot2::ggsave(corplots,
  path = here::here("output", "images", "descriptives"),
  filename = "corplot.png", dpi = 1200, device = "png"
)

### PLOTS: Delay Discounting ------------------------------------------------------------------------------------------------

#### Plot: Delay discounting ~ mental health indicators ---------------------------------------------------------------------

# BIS - Behavioural inhibition subscale

lmer_fit <- lmerTest::lmer(bisbas_bis ~ logk + (1 | subjID), data = ddtvar_long %>% filter(!is.na(logk), !is.na(bisbas_bis)))

# simplistic model across all individuals (for plotting sake)
lm_fit <- lm(bisbas_bis ~ logk, data = ddtvar_long)
std_coef <- effectsize::standardize_parameters(lmer_fit, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_fit), digits = 5)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_fit)

# get predicted bis score
pred <- ggeffects::ggpredict(lmer_fit, terms = "logk")
print(pred, digits = 3)

# plot the relationship across participants
dd_bis <- ddtvar_long %>%
  filter(!is.na(logk) & !is.na(bisbas_bis)) %>%
  mutate(residuals = abs(bisbas_bis - predict(lm_fit))) %>%
  ggplot(., aes(x = logk, y = bisbas_bis, color = residuals)) +
  geom_point(alpha = .75, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .01)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1, colour = "#2E2E2E") +
  scale_colour_viridis(begin = .1, end = .8, option = "C", direction = 1) +
  plot_theme +
  aspect_ratio_square +
  labs(y = "BIS", x = "Delay discounting (-logk)") +
  scale_x_continuous(
    breaks = seq(0, 10, 2), expand = c(0.04, 0)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 10), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 30, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# BAS - Behavioural activation subscale

lmer_fit <- lmerTest::lmer(bisbas_bas ~ logk + (1 | subjID), data = ddtvar_long %>% filter(!is.na(logk), !is.na(bisbas_bas)))

# simplistic model across all individuals (for plotting sake)
lm_fit <- lm(bisbas_bas ~ logk, data = ddtvar_long)
std_coef <- effectsize::standardize_parameters(lmer_fit, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_fit), digits = 5)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_fit)

# get predicted bis score
pred <- ggeffects::ggpredict(lmer_fit, terms = "logk")
print(pred, digits = 3)

# plot the relationship across participants

dd_bas <- ddtvar_long %>%
  filter(!is.na(logk) & !is.na(bisbas_bas)) %>%
  mutate(residuals = abs(bisbas_bas - predict(lm_fit))) %>%
  ggplot(., aes(x = logk, y = bisbas_bas, color = residuals)) +
  geom_point(alpha = .75, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .01)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1, colour = "#2E2E2E") +
  scale_colour_viridis(begin = .1, end = .8, option = "C", direction = 1) +
  plot_theme +
  aspect_ratio_square +
  labs(y = "BAS", x = "Delay discounting (-logk)") +
  scale_x_continuous(
    breaks = seq(0, 10, 2), expand = c(0.04, 0)) +
  scale_y_continuous(limits = c(20, 52), breaks = seq(20, 50, 10), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 20, yend = 50, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# SDQ - Strength and difficulties questionnaire

lmer_fit <- lmerTest::lmer(sdq_total ~ logk + (1 | subjID), data = ddtvar_long %>% filter(!is.na(logk), !is.na(sdq_total)))

# simplistic model across all individuals (for plotting sake)
lm_fit <- lm(sdq_total ~ logk, data = ddtvar_long)
std_coef <- effectsize::standardize_parameters(lmer_fit, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_fit), digits = 5)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_fit)

# get predicted bis score
pred <- ggeffects::ggpredict(lmer_fit, terms = "logk")
print(pred, digits = 3)

# plot the relationship across participants

dd_sdq <- ddtvar_long %>%
  filter(!is.na(logk) & !is.na(sdq_total)) %>%
  mutate(residuals = abs(sdq_total - predict(lm_fit))) %>%
  ggplot(., aes(x = logk, y = sdq_total, color = residuals)) +
  geom_point(alpha = .75, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .01)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1, colour = "#2E2E2E") +
  scale_colour_viridis(begin = .1, end = .8, option = "C", direction = 1) +
  plot_theme +
  aspect_ratio_square +
  labs(y = "SDQ", x = "Delay discounting (-logk)") +
  scale_x_continuous(
    breaks = seq(0, 10, 2), expand = c(0.04, 0)) +
  scale_y_continuous(limits = c(10, 40), breaks = seq(10, 40, 10), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# HSCS - Highly sensitive child scale

lmer_fit <- lmerTest::lmer(hscs_total ~ logk + (1 | subjID), data = ddtvar_long %>% filter(!is.na(logk), !is.na(hscs_total)))

# simplistic model across all individuals (for plotting sake)
lm_fit <- lm(hscs_total ~ logk, data = ddtvar_long)
std_coef <- effectsize::standardize_parameters(lmer_fit, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_fit), digits = 5)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_fit)

# get predicted bis score
pred <- ggeffects::ggpredict(lmer_fit, terms = "logk")
print(pred, digits = 3)

# plot the relationship across participants

dd_hscs <- ddtvar_long %>%
  filter(!is.na(logk) & !is.na(hscs_total)) %>%
  mutate(residuals = abs(hscs_total - predict(lm_fit))) %>%
  ggplot(., aes(x = logk, y = hscs_total, color = residuals)) +
  geom_point(alpha = .75, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .01)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1, colour = "#2E2E2E") +
  scale_colour_viridis(begin = .1, end = .8, option = "C", direction = 1) +
  plot_theme +
  aspect_ratio_square +
  labs(y = "HSCS", x = "Delay discounting (-logk)") +
  scale_x_continuous(
    breaks = seq(0, 10, 2), expand = c(0.04, 0)) +
  scale_y_continuous(limits = c(10, 92), breaks = seq(10, 90, 20), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 90, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# EATQ-EC - Early adolescent temperament questionnaire effortful control subscale

lmer_fit <- lmerTest::lmer(eatq_ec_total ~ logk + (1 | subjID), data = ddtvar_long %>% filter(!is.na(logk), !is.na(eatq_ec_total)))

# simplistic model across all individuals (for plotting sake)
lm_fit <- lm(eatq_ec_total ~ logk, data = ddtvar_long)
std_coef <- effectsize::standardize_parameters(lmer_fit, method = "refit")

# examine results and standardised coefficients
print(summary(lmer_fit), digits = 5)
print(std_coef, digits = 5)

# get R square
MuMIn::r.squaredGLMM(lmer_fit)

# get predicted bis score
pred <- ggeffects::ggpredict(lmer_fit, terms = "logk")
print(pred, digits = 3)

# plot the relationship across participants

dd_eatq_ec <- ddtvar_long %>%
  filter(!is.na(logk) & !is.na(eatq_ec_total)) %>%
  mutate(residuals = abs(eatq_ec_total - predict(lm_fit))) %>%
  ggplot(., aes(x = logk, y = eatq_ec_total, color = residuals)) +
  geom_point(alpha = .75, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .01)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1, colour = "#2E2E2E") +
  scale_colour_viridis(begin = .1, end = .8, option = "C", direction = 1) +
  plot_theme +
  aspect_ratio_square +
  labs(y = "EATQ-EC", x = "Delay discounting (-logk)") +
  scale_x_continuous(
    breaks = seq(0, 10, 2), expand = c(0.04, 0)) +
  scale_y_continuous(limits = c(40, 70), breaks = seq(40, 70, 10), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 40, yend = 70, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# merge above plots into a single visualisation
dd_mh <- ggpubr::ggarrange(
  dd_bis + theme(axis.title.x = element_blank(), axis.title.y = element_text(face = "bold")),
  dd_bas + theme(axis.title.x = element_blank(), axis.title.y = element_text(face = "bold")),
  dd_sdq + theme(axis.title.x = element_blank(), axis.title.y = element_text(face = "bold")),
  dd_hscs + theme(axis.title.x = element_blank(), axis.title.y = element_text(face = "bold")),
  dd_eatq_ec + theme(axis.title.x = element_blank(), axis.title.y = element_text(face = "bold")),
  heights = c(.25, .5, .5, .5, .5),
  widths = c(1, 1, 1, 1, 1, .2),
  ncol = 6,
  labels = c("A", "B", "C", "D", "E", ""), label.y = .725,
  align = "v"
)

dd_mh <- ggpubr::annotate_figure(dd_mh,
                        bottom = ggpubr::text_grob("Delay discounting (-logk)",
                                                   face = "bold",
                                                   size = 11,
                                                   vjust = -16))

ggsave(dd_mh,
  path = here::here("output", "images", "descriptives"),
  filename = "dd_mh.png", dpi = 1200, device = "png"
)





#### PLOT: Delay discounting ~ socio-economic status ------------------------------------------------------------------------

ddtvar_long %>%
  mutate(
    ses = as.factor(ses)
  ) %>%
  filter(!is.na(ses), !is.na(logk)) %>%
  raincloud_plot(.,
    predictor_var = "ses", outcome_var = "logk", direction = "vertical",
    xlab = "ses", ylab = "logk", title = "", include_grouping = FALSE, predictor_tick_lab = c("1", "2", "3")
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 10)
  ) +
  theme(panel.background = element_blank(), axis.line.x = element_blank()) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = 10) +
  annotate(geom = "segment", x = 1, xend = 5, y = -Inf, yend = -Inf)

#### PLOT: Delay discounting ~ age ------------------------------------------------------------------------------------------

lm_fit <- lm(logk ~ age, data = ddtvar_long %>% filter(!is.na(logk), !is.na(age)))

dd_age_logk <- ddtvar_long %>%
  mutate(residuals = abs(logk - predict(lm_fit))) %>%
  filter(!is.na(logk)) %>%
  ggplot(., aes(x = age, y = logk, color = residuals)) +
  geom_point(alpha = .75, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .1)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1) +
  scale_colour_viridis(end = 1) +
  plot_theme_legend +
  aspect_ratio_balanced +
  labs(x = "Age", y = "Discounting parameter (logk)") +
  scale_x_continuous(
    breaks = seq(8, 15, 1), expand = c(0.04, 0),
    limits = c(ddtvar_long %>% filter(!is.na(age), !is.na(logk)) %>% select(age) %>% min(.), 15)
  ) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.04, 0)) +
  annotate(x = 8, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 12, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ggplot2::ggsave(dd_age_logk,
  path = here::here("output", "images", "modeling"),
  filename = "age_logk_dot.png", dpi = 1200, device = "png"
)

#### PLOT: Delay discounting individual-level trajectories ------------------------------------------------------------------

dd_indiv_traj <- ddtvar_long %>%
  filter(!is.na(logk)) %>%
  group_by(subjID) %>%
  mutate(subjID = as.factor(subjID)) %>%
  ggplot(., aes(x = age, y = logk, colour = subjID)) +
  geom_point(alpha = 0.1, shape = 16, stroke = 0.4, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, color = "#2E2E2E", se = TRUE) +
  geom_line(aes(group = subjID), alpha = 0.3, size = .5) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Age", y = "log(k)") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d(direction = -1) +
  scale_x_continuous(limits = c(7, 15), breaks = seq(7, 15, 1)) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.04, 0)) +
  scale_fill_viridis_d() +
  annotate(x = 7, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 12, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ggplot2::ggsave(dd_indiv_traj,
  path = here::here("output", "images", "modeling"),
  filename = "dd_indiv_trajectories.png", dpi = 1200, device = "png"
)

#### PLOT: Delay discounting ~ social media use -----------------------------------------------------------------------------

lm_fit <- lm(sm_postandscroll ~ logk, data = ddtvar_long %>% filter(!is.na(logk), !is.na(sm_postandscroll)))

dd_sm_logk <- ddtvar_long %>%
  mutate(residuals = abs(sm_postandscroll - predict(lm_fit))) %>%
  filter(!is.na(sm_postandscroll) & !is.na(logk)) %>% # remove missing values
  ggplot(aes(x = logk, y = sm_postandscroll, colour = residuals)) +
  geom_point(alpha = .9, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .5)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#2E2E2E", size = .5, span = .5, fullrange = TRUE) +
  scale_colour_viridis(direction = 1, end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting log(k)", y = "Posting and scrolling") +
  scale_y_continuous(breaks = seq(0, 5, 1), expand = c(0.04, 0)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = .75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 5, colour = "#2E2E2E", lwd = .75, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    # remove plot outline
    plot.background = element_rect(fill = "transparent", colour = NA_character_)
  ) +
  stat_poly_eq(use_label(c("eq")), label.x = .9, label.y = .9)

ggplot2::ggsave(dd_sm_logk,
  path = here::here("output", "images", "modeling"),
  filename = "dd_sm_logk_smooth.png", dpi = 1200, device = "png"
)

#### PLOT: Delay discounting ~ Compulsive Internet Use ------------------------------------------------------------------

lm_fit <- lm(cius_total ~ logk, data = ddtvar_long %>% filter(!is.na(logk), !is.na(cius_total)))

# across waves
dd_cius_logk <- ddtvar_long %>%
  mutate(residuals = abs(cius_total - predict(lm_fit))) %>%
  filter(!is.na(cius_total) & !is.na(logk)) %>% # remove missing values
  ggplot(aes(x = logk, y = cius_total, colour = residuals)) +
  geom_point(alpha = .9, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .5)) +
  geom_smooth(method = stats::lm, formula = y ~ x, colour = "#2E2E2E", size = .5, span = .5, fullrange = TRUE) +
  scale_colour_viridis(direction = 1, end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting log(k)", y = "CIUS") +
  scale_y_continuous(breaks = seq(10, 40, 5), expand = c(0.04, 0)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = .75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = .75, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA_character_) # necessary to avoid drawing plot outline
  ) +
  stat_poly_eq(use_label(c("eq")), label.x = .9, label.y = .9)

# wave 5
dd_cius_logk_5 <- ddtvar_long %>%
  filter(wave == 5) %>%
  filter(!is.na(cius_total) & !is.na(logk)) %>% # remove missing values
  ggplot(aes(x = logk, y = cius_total, colour = cius_total)) +
  geom_point(alpha = .9, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .5)) +
  geom_smooth(method = stats::lm, formula = y ~ x, colour = "#2E2E2E", size = .5, span = .5, fullrange = TRUE) +
  scale_colour_viridis(direction = 1, end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting (logk)", y = "Compulsive SM use") +
  scale_y_continuous(breaks = seq(10, 40, 5), expand = c(0.04, 0)) +
  scale_x_continuous(limits = c(0, 11.5), breaks = seq(0, 11.5, 1), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 11, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA_character_) # necessary to avoid drawing plot outline
  ) +
  stat_poly_eq(use_label(c("eq")), label.x = .9, label.y = .9)

# wave 6
dd_cius_logk_6 <- ddtvar_long %>%
  filter(wave == 6) %>%
  filter(!is.na(cius_total) & !is.na(logk)) %>% # remove missing values
  ggplot(aes(x = logk, y = cius_total, colour = cius_total)) +
  geom_point(alpha = .9, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .5)) +
  geom_smooth(method = stats::lm, formula = y ~ x, colour = "#2E2E2E", size = .5, span = .5, fullrange = TRUE) +
  scale_colour_viridis(direction = 1, end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting (logk)", y = "Compulsive SM use") +
  scale_y_continuous(breaks = seq(10, 40, 5), expand = c(0.04, 0)) +
  scale_x_continuous(limits = c(0, 11.5), breaks = seq(0, 11.5, 1), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 11, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 1, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = 1, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA_character_) # necessary to avoid drawing plot outline
  ) +
  stat_poly_eq(use_label(c("eq")), label.x = .9, label.y = .9)


ggplot2::ggsave(dd_cius_logk,
  path = here::here("output", "images", "modeling"),
  filename = "dd_cius_logk_smooth.png", dpi = 1200, device = "png"
)
ggplot2::ggsave(dd_cius_logk_5,
  path = here::here("output", "images", "modeling"),
  filename = "dd_cius_logk5_smooth.png", dpi = 1200, device = "png"
)
ggplot2::ggsave(dd_cius_logk_6,
  path = here::here("output", "images", "modeling"),
  filename = "dd_cius_logk6_smooth.png", dpi = 1200, device = "png"
)

#### PLOT: Delay discounting ~ social media use grouped by response to social media questionnaire ---------------------------

# posting and scrolling
dd_den_po <- ddtvar_long %>%
  drop_na(logk) %>%
  drop_na(sm_postandscroll) %>%
  mutate(sm_postandscroll = as_factor(sm_postandscroll)) %>%
  ggplot(., aes(x = logk, y = sm_postandscroll, fill = sm_postandscroll)) +
  geom_density_ridges() +
  scale_fill_viridis_d(direction = -1, end = .95) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting (logk)", y = "Posting and scrolling") +
  scale_y_discrete(breaks = seq(0, 5, 1), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.03, 0)) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# messaging
dd_den_me <- ddtvar_long %>%
  drop_na(logk) %>%
  drop_na(sm_messaging) %>%
  mutate(sm_messaging = as_factor(sm_messaging)) %>%
  ggplot(., aes(x = logk, y = sm_messaging, fill = sm_messaging)) +
  geom_density_ridges() +
  scale_fill_viridis_d(direction = -1, end = .95) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting (logk)", y = "Messaging") +
  scale_y_discrete(breaks = seq(0, 5, 1), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.03, 0)) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# videos
dd_den_vi <- ddtvar_long %>%
  drop_na(logk) %>%
  drop_na(sm_video) %>%
  mutate(sm_video = as_factor(sm_video)) %>%
  ggplot(., aes(x = logk, y = sm_video, fill = sm_video)) +
  geom_density_ridges() +
  scale_fill_viridis_d(direction = -1, end = .95) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting (logk)", y = "Watching videos") +
  scale_y_discrete(breaks = seq(0, 5, 1), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.03, 0)) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# video calling
dd_den_vc <- ddtvar_long %>%
  drop_na(logk) %>%
  drop_na(sm_videocall) %>%
  mutate(sm_videocall = as_factor(sm_videocall)) %>%
  ggplot(., aes(x = logk, y = sm_videocall, fill = sm_videocall)) +
  geom_density_ridges() +
  scale_fill_viridis_d(direction = -1, end = .95) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting (logk)", y = "Video calling") +
  scale_y_discrete(breaks = seq(0, 5, 1), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.03, 0)) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# arrange all social media plots together
dd_sm_logk_dens <- ggarrange(dd_den_me, dd_den_vi, dd_den_po, dd_den_vc, ncol = 2, nrow = 2)
ggplot2::ggsave(dd_sm_logk_dens, path = here::here("output", "images"), filename = "sm_logk_dens.png", dpi = 1200, device = "png")

### PLOTS: Delay Discounting (beta) -----------------------------------------------------------------------------------------

#### PLOT: Correlation matrix of delay discounting (beta) -------------------------------------------------------------------

# wave 5
cors <- ddtvar_wide %>%
  select(
    subjID, w05_estimate_beta, w05_bisbas_total, w05_bisbas_bis, w05_bisbas_bas,
    w05_sdq_total, w05_hscs_total, w05_eatq_ec_total
  ) %>%
  corrr::correlate(., method = "pearson", use = "pairwise.complete.obs") %>%
  corrr::rearrange(.)

corplots <- cors %>%
  corrr::rplot(., print_cor = .05) +
  aspect_ratio_wide +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot2::ggsave(corplots,
  path = here::here("output", "images", "descriptives"),
  filename = "beta_corplot_w05.png", dpi = 1200, device = "png"
)

# wave 6
cors <- ddtvar_wide %>%
  select(
    subjID, w06_estimate_beta, w06_bisbas_total, w06_bisbas_bis, w06_bisbas_bas,
    w06_sdq_total, w06_hscs_total, w06_eatq_ec_total
  ) %>%
  corrr::correlate(., method = "pearson", use = "pairwise.complete.obs") %>%
  corrr::rearrange(.)

corplots <- cors %>%
  corrr::rplot(., print_cor = .05) +
  aspect_ratio_wide +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot2::ggsave(corplots,
  path = here::here("output", "images", "descriptives"),
  filename = "beta_corplot_w06.png", dpi = 1200, device = "png"
)

#### PLOT: Inverse temperature (beta) ~ age ---------------------------------------------------------------------------------

lm_fit <- lm(estimate_beta ~ age, data = ddtvar_long %>% filter(!is.na(estimate_beta), !is.na(age)))

dd_age_beta <- ddtvar_long %>%
  mutate(residuals = abs(estimate_beta - predict(lm_fit))) %>%
  filter(!is.na(estimate_beta)) %>%
  ggplot(., aes(x = age, y = estimate_beta, color = residuals)) +
  geom_point(alpha = .75, shape = 16, stroke = .4, size = 2, position = position_jitter(seed = 1, width = .1)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, span = 1) +
  scale_colour_viridis(end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Age", y = "Inverse temperature (beta)") +
  scale_x_continuous(
    breaks = seq(8, 15, 1), expand = c(0.04, 0),
    limits = c(ddtvar_long %>% filter(!is.na(age), !is.na(logk)) %>% select(age) %>% min(.), 15)
  ) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5), expand = c(0.04, 0)) +
  annotate(x = 8, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 2.5, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ggplot2::ggsave(dd_age_beta,
  path = here::here("output", "images", "modeling"),
  filename = "dd_age_beta_dot.png", dpi = 1200, device = "png"
)

#### PLOT: Individual trajectories in beta inverse temperature --------------------------------------------------------------

dd_indiv_traj_beta <- ddtvar_long %>%
  filter(!is.na(estimate_beta)) %>%
  group_by(subjID) %>%
  mutate(subjID = as.factor(subjID)) %>%
  ggplot(., aes(x = age, y = estimate_beta, colour = subjID)) +
  geom_point(alpha = 0.1, shape = 16, stroke = 0.4, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, size = .5, color = "#2E2E2E", se = TRUE) +
  geom_line(aes(group = subjID), alpha = 0.3, size = .5) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Age", y = "log(k)") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d(direction = -1) +
  scale_x_continuous(limits = c(7, 15), breaks = seq(7, 15, 1)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5), expand = c(0.04, 0)) +
  scale_fill_viridis_d() +
  annotate(x = 7, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 12, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ggplot2::ggsave(dd_indiv_traj_beta,
  path = here::here("output", "images", "modeling"),
  filename = "dd_beta_indiv_trajectories.png", dpi = 1200, device = "png"
)

#### PLOT: Inverse temperature (beta) ~ social media use --------------------------------------------------------------------

lm_fit_beta <- lm(sm_postandscroll ~ estimate_beta, data = ddtvar_long %>% filter(!is.na(estimate_beta), !is.na(sm_postandscroll)))

dd_sm_beta <- ddtvar_long %>%
  mutate(residuals = abs(sm_postandscroll - predict(lm_fit_beta))) %>%
  filter(!is.na(sm_postandscroll) & !is.na(estimate_beta)) %>% # remove missing values
  ggplot(aes(x = estimate_beta, y = sm_postandscroll, colour = residuals)) +
  geom_point(alpha = .9, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .5)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#2E2E2E", size = .5, span = .5, fullrange = TRUE) +
  scale_colour_viridis(direction = 1, end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Inverse temperature (beta)", y = "Posting and scrolling") +
  scale_y_continuous(breaks = seq(0, 2, 1), expand = c(0.04, 0)) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, .5), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 2, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 1, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 5, colour = "#2E2E2E", lwd = 1, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA_character_) # necessary to avoid drawing plot outline
  ) +
  stat_poly_eq(use_label(c("eq")), label.x = .9, label.y = .9)

ggplot2::ggsave(dd_sm_beta,
  path = here::here("output", "images", "modeling"),
  filename = "dd_sm_beta_smooth.png", dpi = 1200, device = "png"
)

#### PLOT: Inverse temperature (beta) ~ Compulsive Internet Use ---------------------------------------------------------

lm_fit_beta <- lm(cius_total ~ estimate_beta, data = ddtvar_long %>% filter(!is.na(estimate_beta), !is.na(cius_total)))

# across waves
dd_cius_beta <- ddtvar_long %>%
  mutate(residuals = abs(cius_total - predict(lm_fit_beta))) %>%
  filter(!is.na(cius_total) & !is.na(estimate_beta)) %>% # remove missing values
  ggplot(aes(x = estimate_beta, y = cius_total, colour = residuals)) +
  geom_point(alpha = .9, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .5)) +
  geom_smooth(method = stats::lm, formula = y ~ x, colour = "#2E2E2E", size = .5, span = .5, fullrange = TRUE) +
  scale_colour_viridis(direction = 1, end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Inverse temperature (beta)", y = "CIUS") +
  scale_y_continuous(breaks = seq(10, 40, 5), expand = c(0.04, 0)) +
  scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, .5), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 1.5, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = .75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = .75, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA_character_) # necessary to avoid drawing plot outline
  ) +
  stat_poly_eq(use_label(c("eq")), label.x = .9, label.y = .9)

# wave 5
dd_cius_beta_5 <- ddtvar_long %>%
  filter(wave == 5) %>%
  filter(!is.na(cius_total) & !is.na(estimate_beta)) %>% # remove missing values
  ggplot(aes(x = estimate_beta, y = cius_total, colour = cius_total)) +
  geom_point(alpha = .9, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .5)) +
  geom_smooth(method = stats::lm, formula = y ~ x, colour = "#2E2E2E", size = .5, span = .5, fullrange = TRUE) +
  scale_colour_viridis(direction = 1, end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting parameter (logk)", y = "Compulsive SM use") +
  scale_y_continuous(breaks = seq(10, 40, 5), expand = c(0.04, 0)) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, .5), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 2, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA_character_) # necessary to avoid drawing plot outline
  ) +
  stat_poly_eq(use_label(c("eq")), label.x = .9, label.y = .9)

# wave 6
dd_cius_beta_6 <- ddtvar_long %>%
  filter(wave == 6) %>%
  filter(!is.na(cius_total) & !is.na(estimate_beta)) %>% # remove missing values
  ggplot(aes(x = estimate_beta, y = cius_total, colour = cius_total)) +
  geom_point(alpha = .9, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .5)) +
  geom_smooth(method = stats::lm, formula = y ~ x, colour = "#2E2E2E", size = .5, span = .5, fullrange = TRUE) +
  scale_colour_viridis(direction = 1, end = 1) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting parameter (logk)", y = "Compulsive SM use") +
  scale_y_continuous(breaks = seq(10, 40, 5), expand = c(0.04, 0)) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, .5), expand = c(0.04, 0)) +
  annotate(x = 0, xend = 2, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 1, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 10, yend = 40, colour = "#2E2E2E", lwd = 1, geom = "segment") +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA_character_) # necessary to avoid drawing plot outline
  ) +
  stat_poly_eq(use_label(c("eq")), label.x = .9, label.y = .9)

ggplot2::ggsave(dd_cius_beta,
  path = here::here("output", "images", "modeling"),
  filename = "dd_cius_beta_smooth.png", dpi = 1200, device = "png"
)

ggplot2::ggsave(dd_cius_beta_5,
  path = here::here("output", "images", "modeling"),
  filename = "dd_cius_beta5_smooth.png", dpi = 1200, device = "png"
)

ggplot2::ggsave(dd_cius_beta_6,
  path = here::here("output", "images", "modeling"),
  filename = "dd_cius_beta6_smooth.png", dpi = 1200, device = "png"
)

#### PLOT: Inverse temperature (beta) ~ social media use grouped ------------------------------------------------------------

# posting and scrolling
dd_den_po_b <- ddtvar_long %>%
  drop_na(estimate_beta) %>%
  drop_na(sm_postandscroll) %>%
  mutate(sm_postandscroll = as_factor(sm_postandscroll)) %>%
  ggplot(., aes(x = estimate_beta, y = sm_postandscroll, fill = sm_postandscroll)) +
  geom_density_ridges() +
  scale_fill_viridis_d(direction = -1, end = .95) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Inverse Temperature (beta)", y = "Posting and scrolling") +
  scale_y_discrete(breaks = seq(0, 5, 1), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, .5), expand = c(0.03, 0)) +
  annotate(x = 0, xend = 2, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# messaging
dd_den_me_b <- ddtvar_long %>%
  drop_na(estimate_beta) %>%
  drop_na(sm_messaging) %>%
  mutate(sm_messaging = as_factor(sm_messaging)) %>%
  ggplot(., aes(x = estimate_beta, y = sm_messaging, fill = sm_messaging)) +
  geom_density_ridges() +
  scale_fill_viridis_d(direction = -1, end = .95) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Inverse Temperature (beta)", y = "Messaging") +
  scale_y_discrete(breaks = seq(0, 5, 1), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, .5), expand = c(0.03, 0)) +
  annotate(x = 0, xend = 2, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# video watching
dd_den_vi_b <- ddtvar_long %>%
  drop_na(estimate_beta) %>%
  drop_na(sm_video) %>%
  mutate(sm_video = as_factor(sm_video)) %>%
  ggplot(., aes(x = estimate_beta, y = sm_video, fill = sm_video)) +
  geom_density_ridges() +
  scale_fill_viridis_d(direction = -1, end = .95) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting parameter (logk)", y = "Watching videos") +
  scale_y_discrete(breaks = seq(0, 5, 1), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, .5), expand = c(0.03, 0)) +
  annotate(x = 0, xend = 2, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# video calling
dd_den_vc_b <- ddtvar_long %>%
  drop_na(estimate_beta) %>%
  drop_na(sm_videocall) %>%
  mutate(sm_videocall = as_factor(sm_videocall)) %>%
  ggplot(., aes(x = estimate_beta, y = sm_videocall, fill = sm_videocall)) +
  geom_density_ridges() +
  scale_fill_viridis_d(direction = -1, end = .95) +
  plot_theme +
  aspect_ratio_balanced +
  labs(x = "Delay discounting parameter (logk)", y = "Video calling") +
  scale_y_discrete(breaks = seq(0, 5, 1), expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, .5), expand = c(0.03, 0)) +
  annotate(x = 0, xend = 2, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = -Inf, xend = -Inf, y = 1, yend = 6, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

dd_sm_beta_dens <- ggpubr::ggarrange(dd_den_me_b, dd_den_vi_b, dd_den_po_b, dd_den_vc_b, ncol = 2, nrow = 2)

ggplot2::ggsave(dd_sm_beta_dens,
  path = here::here("output", "images"),
  filename = "dd_sm_beta_dens.png", dpi = 1200, device = "png"
)
