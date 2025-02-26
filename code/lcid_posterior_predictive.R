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

SIMULATE_DATA <- TRUE  # should data be simulated or a previous simulation loaded
FIT_CLUSTER <- FALSE   # should the fitting be run on a cluster or locally
FIT_MODEL <- TRUE      # should the fitting be run or loaded from file
N_SAMPLE <- 10000       # number of participants to simulate

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

# Combine the results into a single table
ppc_table <- bind_rows(ppc02_corr, ppc03_corr, ppc04_corr, ppc05_corr, ppc06_corr) %>%
  knitr::kable(., caption = "Correlations of predicted and real choices", align = "l", digits = 3)

## Visualise Posterior Predictive Checks -------------------------------------------------------------------------------------
ppc02 %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice)) +
  geom_point(shape = 20, alpha = .8, aes(colour = hdi_upper_pred_choice - hdi_lower_pred_choice),
             position = position_jitter(width = .05)) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis(name = "HDI Width", direction = 1) +
  scale_x_continuous(breaks = seq(0,1,.2), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0,1,.2), expand = c(0.05, 0.05)) +
  labs(x = "Observed Choice", y = "Predicted Choice") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  stat_poly_eq(use_label(c("R2")), label.y = 0.925, size = 8, colour = "#2E2E2E", parse = TRUE)

ppc_plots <- lapply(list(ppc02, ppc03, ppc04, ppc05, ppc06), FUN = ppc_plot)
ppcs <- list(ppc02, ppc03, ppc04, ppc05, ppc06)
ppc_plots <- list()
for(x in 1:5){
  ppc_plots[[x]] <- ppc_plot(ppcs[[x]]) +
    ggtitle(paste("Wave", x+1, sep = " ")) +
    theme(plot.title = element_text(vjust = -1, size = 10))
}

ppc_plot(ppc02)


blank <- ggplot() + theme_void()
ppc_plots <- list(ppc_plots[[1]] + rremove("xlab"),
                  ppc_plots[[2]] + rremove("xlab") + rremove("ylab"),
                  ppc_plots[[3]] + rremove("xlab") + rremove("ylab"),
                  ppc_plots[[4]],
                  ppc_plots[[5]] + rremove("ylab"))


ppc_plots_combined <- ggpubr::ggarrange(
  ggpubr::ggarrange(ppc_plots[[1]], ppc_plots[[2]], ppc_plots[[3]], blank, nrow = 1,
            widths = c(1, 1, 1, 0), legend = "none"),
  ggpubr::ggarrange(ppc_plots[[4]], ppc_plots[[5]], blank, blank, nrow = 1,
            widths = c(1, 1, 0.5,0.5), legend = "none"),
  ggpubr::as_ggplot(ggpubr::get_legend(ppc_plots[[1]] , position = "bottom")),
  nrow = 3, heights = c(1, 1, 0.25),
  legend = "none"
)

ggsave(ppc_plots_combined,
       path = here::here("output", "lcid", "dd_delaydiscount", "images", "08_posterior_predictive"),
       filename = "ppc_corrplots.png", dpi = 1200, device = "png")

ggppc1 <- ppc_plots[[1]]; ggppc2 <- ppc_plots[[2]]; ggppc3 <- ppc_plots[[3]]; ggppc4 <- ppc_plots[[4]]; ggppc5 <- ppc_plots[[5]]

ggsave(ggppc1, path = here::here("output", "lcid", "dd_delaydiscount", "images", "08_posterior_predictive"),
       filename = "ppc_02.png", dpi = 1200, device = "png")
ggsave(ggppc2, path = here::here("output", "lcid", "dd_delaydiscount", "images", "08_posterior_predictive"),
       filename = "ppc_03.png", dpi = 1200, device = "png")
ggsave(ggppc3, path = here::here("output", "lcid", "dd_delaydiscount", "images", "08_posterior_predictive"),
       filename = "ppc_04.png", dpi = 1200, device = "png")
ggsave(ggppc4, path = here::here("output", "lcid", "dd_delaydiscount", "images", "08_posterior_predictive"),
       filename = "ppc_05.png", dpi = 1200, device = "png")
ggsave(ggppc5, path = here::here("output", "lcid", "dd_delaydiscount", "images", "08_posterior_predictive"),
       filename = "ppc_06.png", dpi = 1200, device = "png")

