## ======================================================================================================================= ##
## Script:    POSTERIOR PREDICTIVE CHECK
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Mon Jul 29 13:58:29 2024
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

library(renv)
renv::restore()

set.seed(777)

# set global parameters determining the model fitting process

SIMULATE_DATA <- FALSE # should data be simulated or a previous simulation loaded
FIT_CLUSTER <- FALSE # should the fitting be run on a cluster or locally
FIT_MODEL <- FALSE # should the fitting be run or loaded from file
N_SAMPLE <- 10000 # number of participants to simulate

# load required packages
library(loo)
library(here)
library(knitr)
library(purrr)
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(hexbin)
library(stringr)
library(ggplot2)
library(ggpmisc)
library(viridis)
library(magrittr)
library(cmdstanr)
library(hBayesDM)
library(bayesplot)
library(patchwork)
library(ggcorrplot)
library(viridisLite)

here::i_am("renv.lock") # set directory

source(here("code", "functions", "fun_plots.R"))
source(here("code", "functions", "fun_helper.R"))

#### Load data --------------------------------------------------------------------------------------------------------------
ppc02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_02.RDS"))
ppc03 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_03.RDS"))
ppc04 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_04.RDS"))
ppc05 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_05.RDS"))
ppc06 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_06.RDS"))

# CORRELATIONS --------------------------------------------------------------------------------------------------------------
# Correlate posterior predictions with oberved data

ppc02_corr <- ppc_correlate(ppc02, " Wave 2")
ppc03_corr <- ppc_correlate(ppc03, " Wave 3")
ppc04_corr <- ppc_correlate(ppc04, " Wave 4")
ppc05_corr <- ppc_correlate(ppc05, " Wave 5")
ppc06_corr <- ppc_correlate(ppc06, " Wave 6")

print(cor.test(ppc02$mean_real_choice, ppc02$mean_pred_choice), digits = 7)
print(cor.test(ppc03$mean_real_choice, ppc03$mean_pred_choice), digits = 7)
print(cor.test(ppc04$mean_real_choice, ppc04$mean_pred_choice), digits = 7)
print(cor.test(ppc05$mean_real_choice, ppc05$mean_pred_choice), digits = 7)
print(cor.test(ppc06$mean_real_choice, ppc06$mean_pred_choice), digits = 7)

# Combine the results into a single table
ppc_table <- bind_rows(ppc02_corr, ppc03_corr, ppc04_corr, ppc05_corr, ppc06_corr) %>%
  knitr::kable(., caption = "Correlations of predicted and real choices", align = "l", digits = 3)

## Visualise Posterior Predictive Checks -------------------------------------------------------------------------------------
ppcplot2 <- ppc02 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot3 <- ppc03 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot4 <- ppc04 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot5 <- ppc05 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot6 <- ppc06 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot6 <- ppcplot6 +
  theme(legend.position = "bottom") +
  labs(colour = "Delay level") +
  ggtitle("Wave 6") +
  theme(plot.title = element_text(vjust = -2, size = 12))

ppcplot5 <- ppcplot5 +
  ggtitle("Wave 5") +
  theme(plot.title = element_text(vjust = -2, size = 12))

ppcplot4 <- ppcplot4 +
  ggtitle("Wave 4") +
  theme(plot.title = element_text(vjust = -2, size = 12))

ppcplot3 <- ppcplot3 +
  ggtitle("Wave 3") +
  theme(plot.title = element_text(vjust = -2, size = 12))

ppcplot2 <- ppcplot2 +
  ggtitle("Wave 2") +
  theme(plot.title = element_text(vjust = -2, size = 12))

# create layout of mergerd plot
layout <- c(
  area(1, 1),
  area(1, 2),
  area(1, 3),
  area(2, 1),
  area(2, 2)
)

# combine the plots using the defined layout
combined_plot <- ppcplot2 + ppcplot3 + ppcplot4 + ppcplot5 + ppcplot6 + plot_spacer() +
  plot_layout(design = layout, guides = "collect")

ppc_plots_all <- ggpubr::ggarrange(
  ppcplot2, ppcplot3, ppcplot4, ppcplot5, ppcplot6,
  ncol = 2, nrow = 3,
  common.legend = TRUE,    # shared legend
  legend = "bottom",
  legend.grob = get_legend(ppcplot6) # extract legend from the last plot
)

# save plot
ggsave(ppc_plots_all,
       path = here::here("output", "images", "modelfit"),
       filename = "ppc_plots.png",
       width = 6,       # increase width (inches)
       height = 8,       # increase height (inches)
       dpi = 600,        # lower DPI if file size is too large
       device = "png"
)

