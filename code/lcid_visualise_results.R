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
library(purrr)
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(misty)
library(ggpubr)
library(viridis)
library(ggplot2)
library(ggpmisc)
library(ggthemes)
library(parallel)
library(magrittr)
library(tidybayes)
library(ggcorrplot)
library(viridisLite)

# plot settings
options(ggplot2.continuous.colour = "viridis")
options(ggplot2.continuous.fill = "viridis")
theme_set(theme_classic())

# custom functions loaded
source(here("code", "functions", "fun_plots.R"))
source(here("code", "functions", "fun_helper.R"))

### Load Data ---------------------------------------------------------------------------------------------------------------

dat_demographics <- read_csv(here::here("data", "processed", "demographics.csv"))
dd_master_df <- dd_data <- readRDS(here::here("data", "processed", "dd_task.Rds"))

ddtask02 <- dd_data[[1]]
ddtask03 <- dd_data[[2]]
ddtask04 <- dd_data[[3]]
ddtask05 <- dd_data[[4]]
ddtask06 <- dd_data[[5]]

ddtvar_wide <- utils::read.csv(file = here::here("data", "processed", "tvar_masterdat_wide.csv"), header = TRUE)
ddtvar_long <- utils::read.csv(file = here::here("data", "processed", "tvar_masterdat_long.csv"), header = TRUE)

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

dd_hyperbo_check_02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_02.RDS"))
dd_hyperbo_loo_02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_loo_02.RDS"))
dd_hyperbo_params_02 <- readRDS(here::here("output", "modelfit",  "dd_hyperbo_parameters_02.RDS"))
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
  theme(axis.ticks.length.x = unit(0, "cm"),
        axis.text = element_text(size = 12),
        text = element_text(size = 12)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

# data complete in waves 5, 6
dat_demographics %>% # data available for W05 and W06
  filter(w05_participation == 1 & w06_participation == 1) %>%
  nrow()

# data complete in waves 5, 6, 7
dat_demographics %>%
  filter(w05_participation == 1 & w06_participation == 1 & w07_participation == 1) %>%
  nrow()

#### Trial number density ---------------------------------------------------------------------------------------------------

dd_trials_den <- lapply(dd_data, function(x) ggdensity(x, "trial") + dens_theme)

dd_trials_all <- ggarrange(
  plotlist = dd_trials_den, nrow = 3, ncol = 2, label.x = .5,
  labels = c("Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6"),
  font.label = list(size = 10, font = "plain")
)

ggsave(dd_trials_all,
       path = here("output", "images", "descriptives"),
       filename = "trials_distribution.png", dpi = 1200, device = "png"
)

#### Choice density ---------------------------------------------------------------------------------------------------------

dd_choices_den <- lapply(dd_data, function(x) ggdensity(x, "choice") + dens_theme + scale_x_continuous(breaks = c(0, 1)))
dd_choices_all <- ggarrange(
  plotlist = dd_choices_den, nrow = 2, ncol = 3, label.x = .25,
  labels = c("Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6"), font.label = list(size = 10, font = "plain")
)

ggsave(dd_choices_all,
       path = here("output", "images", "descriptives"),
       filename = "choices_distribution.png", dpi = 1200, device = "png"
)

#### Choice proportion by age -----------------------------------------------------------------------------------------------

dd_data_df <- dd_data %>%
  map(~as_tibble(.)) %>%
  bind_rows(.id = "index") %>%
  mutate(wave = as.numeric(str_sub(index, 7, 7))) %>%
  select(subjID, wave, everything(), -index)

# compute responding proportions per participant per wave
dd_data_df %<>%
  group_by(wave, subjID) %>%
  count(choice) %>%
  group_by(wave, subjID) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = choice, values_from = prop)  %>%
  dplyr::rename(prop_sooner = `0`,
                prop_later = `1`)

dd_data_df <- dd_master_df %>%
  select(subjID, wave, age, estimate_k, logk) %>%
  left_join(., dd_data_df, by = c("subjID","wave")) %>%
  select(subjID, age, estimate_k, logk, prop_sooner, prop_later) %>%
  pivot_longer(cols = c("prop_sooner", "prop_later"), names_to = "choice", values_to = "prop")

# Save rds for choice computations
saveRDS(dd_data_df, file = here("data", "processed", "choice_data.RDS"))

# visualise in plot
gg_age_choice <- dd_data_df %>%
  ggplot2::ggplot(., aes(x = age, y = prop, color = choice)) +
  geom_point(alpha = .5, shape = 16, stroke = .4, size = 1.5, position = position_jitter(seed = 1, width = .1)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d(begin = 0.1, end = .8, labels = c("Later", "Sooner")) +
  plot_theme_legend +
  aspect_ratio_balanced +
  labs(x = "Child age", y = "Proportion of Choice", color = "Choice") +
  scale_x_continuous(breaks = seq(8, 15, 1), expand = c(0.04,0),
                     limits = c(dd_data_df %>% filter(!is.na(prop)) %>% select(age) %>% min(.), 15)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0.04,0)) +
  annotate(x= 8, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x= -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  theme(rect = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA_character_),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# save plot
ggsave(gg_age_choice,
       path = here("output", "images"), filename = "age_choice_interaction.png", dpi = 1200, device = "png")

#### Social media -----------------------------------------------------------------------------------------------------------

sm_ps_age <- lm(sm_postandscroll ~ age, data = ddtvar_long)
print(summary(sm_ps_age), digits = 3)

sm_ps_age_bayes <- brms::brm(sm_postandscroll ~ age, brmsfamily("gaussian"),
                             data = ddtvar_long,
                             chains = 4, cores = getOption("mc.cores", 1), iter = 3000, warmup = 1500, thin = 5,

                             # set normal prior on regression coefficients (mean of 0, location of 3)
                             prior = c(
                               prior(normal(0, 3), "b"),
                               prior(normal(0, 3), "Intercept")
                               )
                             )

print(summary(sm_ps_age_bayes), digits = 3)
tidybayes::summarise_draws(sm_ps_age_bayes)
plot(sm_ps_age_bayes)

#### Compulsive social media use --------------------------------------------------------------------------------------------

cius_age <- stats::lm(cius_total ~ age, data = ddtvar_long)
print(summary(sm_ps_age), digits = 3)

cius_age_bayes <- brms::brm(cius_total ~ age, brmsfamily("gaussian"),
                            data = ddtvar_long,
                            chains = 4, cores = getOption("mc.cores", 1), iter = 3000, warmup = 1500, thin = 5,

                            # set normal prior on regression coefficients (mean of 0, location of 3)
                            prior = c(
                              prior(normal(0, 3), "b"),
                              prior(normal(0, 3), "Intercept")
                              )
                            )

print(summary(cius_age_bayes), digits = 3)
tidybayes::summarise_draws(cius_age_bayes)
plot(cius_age_bayes)

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
  xlab("Delay discounting parameter `k`") + ylab("Density") +
  facet_grid(~ wave, labeller = labeller(wave = wave.labs)) +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.spacing = unit(.25, "cm")) +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = .75, geom = "segment")

ggsave(k_dens, path = here("output", "images", "modeling"), filename = "k_density.png", dpi = 1200, device = "png")

# examine distribution of estimated discounting rates log-transformed
logk_dens <- ddtvar_long %>%
  filter(!is.na(estimate_k)) %>%
  group_by(wave) %>%
  mutate(log_k = -log(estimate_k)) %>%
  ggplot(., aes(x = log_k, fill = wave)) +
  geom_density(colour = "#2E2E2E") +
  plot_theme +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0,12,3), labels = drop_leading_zeros) +
  xlab("Delay discounting parameter `log(k)`") + ylab("Density") +
  facet_grid(~ wave, labeller = labeller(wave = wave.labs)) +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.spacing = unit(.25, "cm")) +
  annotate(x = 0, xend = 12, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = .75, geom = "segment")

ggsave(logk_dens, path = here("output", "images", "modeling"), filename = "logk_density.png", dpi = 1200, device = "png")

### Delay Discounting ~ Social Media Use ------------------------------------------------------------------------------------

logk_sm <- lm(sm_postandscroll ~ logk, data = ddtvar_long)
print(summary(logk_sm), digits = 3)

logk_sm_bayes <- brms::brm(sm_postandscroll ~ logk, brmsfamily("gaussian"),
                           data = ddtvar_long,
                           chains = 4, cores = getOption("mc.cores", 1), iter = 3000, warmup = 1500, thin = 5,

                           # set normal prior on regression coefficients and intercept (mean of 0, location of 3)
                           prior = c(
                             prior(normal(0, 3), "b"),
                             prior(normal(0, 3), "Intercept")
                             )
                           )

print(summary(logk_sm_bayes), digits = 3)
tidybayes::summarise_draws(logk_sm_bayes)
plot(logk_sm_bayes)

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
  mutate(wave = c(rep(2, nrow(para_02)),rep(3, nrow(para_03)), rep(4, nrow(para_04)),
                  rep(5, nrow(para_05)), rep(6, nrow(para_06))))

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

ggsave(plot_para_k_develop,
       path = here("output", "images", "modeling"),
       filename = "parameters_development.png", dpi = 1200, device = "png"
       )

### Acceptance Probabilities ------------------------------------------------------------------------------------------------

dd_merge <- readRDS(file = here("data", "processed", "dd_choice_data.RDS"))

cp02 <- choice_plot(ddtask02)
ggsave(cp02, path = here("output", "images", "descriptives"), filename = "dd_choice02.png", dpi = 1200, device = "png")
cp03 <- choice_plot(ddtask03)
ggsave(cp03, path = here("output", "images", "descriptives"), filename = "dd_choice03.png", dpi = 1200, device = "png")
cp04 <- choice_plot(ddtask04)
ggsave(cp04, path = here("output", "images", "descriptives"), filename = "dd_choice04.png", dpi = 1200, device = "png")
cp05 <- choice_plot(ddtask05)
ggsave(cp05, path = here("output", "images", "descriptives"), filename = "dd_choice05.png", dpi = 1200, device = "png")
cp06 <- choice_plot(ddtask06)
ggsave(cp06, path = here("output", "images", "descriptives"), filename = "dd_choice06.png", dpi = 1200, device = "png")

choice_combined <- ggarrange(cp02 + rremove("ylab") + rremove("xlab"),
                             cp03 + rremove("ylab") + rremove("xlab"),
                             cp04 + rremove("ylab") + rremove("xlab"),
                             cp05 + rremove("ylab") + rremove("xlab"),
                             cp06 + rremove("ylab") + rremove("xlab"),
                             labels = NULL, common.legend = TRUE, ncol = 3, nrow = 2,
                             align = "hv", legend = "right", label.x = "c", label.y = .5)

choice_combined <- annotate_figure(choice_combined,
                                   left = grid::textGrob("Proportion of later offers selected (%)",
                                                         rot = 90, vjust = 1, gp = gpar(cex = 1), ),
                                   bottom = grid::textGrob("Amount later - amount sooner", gp = gpar(cex = 1)))

ggsave(choice_combined, path = here("output", "images", "descriptives"),
       filename = "dd_choice_combined.png", dpi = 1200, device = "png")

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










