## ======================================================================================================================= ##
## Script:    PARAMETER RECOVERY
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Sun Jun 30 16:05:44 2024
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
N_SAMPLE <- 10000      # number of participants to simulate

# load required packages
library(loo)
library(here)
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
library(ggcorrplot)
library(viridisLite)

here::i_am("renv.lock") # set directory

source(here("code", "functions", "fun_plots.R"))
source(here("code", "functions", "fun_helper.R"))
source(here("code", "functions", "fun_get_params.R"))
source(here("code", "functions", "fun_task_simulation.R"))
source(here("code", "functions", "fun_convergence_check.R"))
source(here("code", "functions", "fun_model_preprocessing.R"))

## PARAMETER RECOVIERY ======================================================================================================

### Simulate/Load Data ------------------------------------------------------------------------------------------------------
if (SIMULATE_DATA) {

  print("SIMULATING NEW FILE")

  # Delay discounting simulation ----------------------------------------------------------------------------------------------
  dd_sim <- dd_task_simulation(
    discounting_rates = rbeta(N_SAMPLE, shape1 = .75, shape2 = 3),
    inverse_temperatures = runif(n = N_SAMPLE, min = 0, max = 5),
    n_participants = N_SAMPLE
  )

  saveRDS(dd_sim, file = here::here("data", "processed", "dd_sim_data.RDS"))
} else {

  print("LOADING SIMULATED DATA FROM FILE")
  dd_sim <- readRDS(file = here::here("data", "processed", "dd_sim_data.RDS"))
}

# extract simulation objects from list
dd_sim_dat <- dd_sim[[1]]
dd_sim_par <- dd_sim[[2]] %>%
  dplyr::select(subjID, k, beta) %>%
  dplyr::distinct() %>%
  tidyr::pivot_longer(cols = c(k, beta), names_to = "parameter", values_to = "value")

### Model Fitting -----------------------------------------------------------------------------------------------------------

# fit on cluster
if (FIT_MODEL && FIT_CLUSTER) {

  # set cmd_stan installation path to location on cluster (change as required)
  set_cmdstan_path("/group/orben/software/linux/cmdstan/cmdstan-2.33.1")

  # load Model (Linux)
  dd_hyperbolic_stan <- cmdstanr::cmdstan_model(here::here("code", "stan", "linux", "dd_hyperbolic.stan"))

  print("MODEL WILL RUN ON CLUSTER")

# fit on local machine
} else if (FIT_MODEL && !FIT_CLUSTER) {

  print("MODEL WILL RUN ON LOCAL MACHINE, CODE EXECUTION MAY TAKE LONGER")

  # load Model (windows/Mac)
  dd_hyperbolic_stan <- cmdstanr::cmdstan_model(here::here("code", "stan", "dd_hyperbolic.stan"))

# load results from file
} else {

  # output message
  print("DID NOT RUN MODELS, YOU CAN LOAD RESULTS FROM FILE")

  dd_par_pr <- readRDS(file = here::here("output", "parameter_recovery", "dd_precover.RDS"))
  results_list <- readRDS(file = here::here("output", "parameter_recovery", "dd_hyperbo_check_precover.RDS"))
  dd_hyperbo_loo <- readRDS(file = here::here("output", "parameter_recovery", "dd_hyperbo_loo_precover.RDS"))
  dd_hyperbo_parameters <- readRDS(file = here::here("output", "parameter_recovery", "dd_hyperbo_parameters_precover.RDS"))
}

### Preprocess data for model fitting ---------------------------------------------------------------------------------------

process_task_data <- function(task_data) {
  model_preprocessing(
    raw_data = task_data,
    retest = FALSE,
    subjs = unique(task_data$subjID),
    n_subj = length(unique(task_data$subjID)),
    t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_data)[, 2],
    t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_data)[, 2])
  )
}

dd_model_dat <- process_task_data(dd_sim_dat)

### Run Model -----------------------------=---------------------------------------------------------------------------------

if (FIT_MODEL) {

  dd_hyperbo_fit <- dd_hyperbolic_stan$sample(
    data = dd_model_dat,
    refresh = 0, chains = 4, parallel_chains = 4,
    iter_warmup = 2000, iter_sampling = 10000,
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE,
    output_dir = NULL
  )

  ### Model results ---------------------------------------------------------------------------------------------------------

  dd_hyperbo_check <- convergence_check(dd_hyperbo_fit,
                                        params = c("k", "beta"),
                                        Rhat = TRUE, ess = TRUE,
                                        trace_plot = TRUE, rank_hist = FALSE
  )

  dd_hyperbo_check$trace_plot            # trace plot for convergence
  dd_hyperbo_loo <- dd_hyperbo_fit$loo() # LOO for model comparison

  dd_hyperbo_parameters <- get_params(
    subj_id = unique(dd_sim_dat$subjID),
    model_fit = dd_hyperbo_fit,
    n_subj = length(unique(dd_sim_dat$subjID)),
    n_params = 2,
    param_names = c("k", "beta")
  )

  # reformat to long (rec = recovered)
  dd_mod_par <- dd_hyperbo_parameters[[2]] %>%
    dplyr::mutate(
      subjID = subj_id,
      value_rec = estimate,
      hdi_lower_rec = hdi_lower,
      hdi_upper_rec = hdi_upper
    ) %>%
    dplyr::select(-subj_id, -estimate, -hdi_lower, -hdi_upper)

  dd_par_pr <- merge(dd_mod_par, dd_sim_par, by = c("subjID", "parameter"))

  # load recovery fit object from output csvs
  dd_recovery_fit <- cmdstanr::read_cmdstan_csv(
    dd_hyperbo_fit$output_files(),
    variables = c("mu_k", "mu_beta")
  )

  ### Save Model Results ----------------------------------------------------------------------------------------------------
  saveRDS(dd_par_pr, file = here::here("output", "parameter_recovery", "dd_precover.RDS"))
  saveRDS(list(dd_hyperbo_check$Rhat, dd_hyperbo_check$ess, dd_hyperbo_check$trace_plot),
          here::here("output", "parameter_recovery", "dd_hyperbo_check_precover.RDS"))
  saveRDS(dd_hyperbo_loo, here::here("output", "parameter_recovery", "dd_hyperbo_loo_precover.RDS"))
  saveRDS(dd_hyperbo_parameters, here::here("output", "parameter_recovery", "dd_hyperbo_parameters_precover.RDS"))

  ### Visualise Model Results -----------------------------------------------------------------------------------------------
  params <- c("k", "beta")
  mu_params <- paste("mu", params, sep = "_")

  # set colour theme for plots
  magma_pal <- viridisLite::magma(6, begin = 0.5, end = 0.8, direction = -1)
  magma_pal %<>% stringr::str_replace(., "FF$", "")

  bayesplot::bayesplot_theme_set(cowplot::theme_half_open(font_family = "", font_size = 11))
  bayesplot::color_scheme_set(magma_pal)

  #### Trace plot -----------------------------------------------------------------------------------------------------------
  ggt_recovery <- bayesplot::mcmc_trace(dd_recovery_fit$post_warmup_draws) +
    trace_theme +
    expand_limits(y = 0, x = 0) +
    theme(aspect.ratio = 1 / 2) +
    scale_x_continuous(breaks = seq(0, nrow(dd_recovery_fit$post_warmup_draws), length.out = 3))

  ggsave(ggt_recovery,
         dpi = 1200, device = "png",
         path = here::here("output", "parameter_recovery", "images"),
         filename = "ggt_precover.png"
         )

  #### Density plot ---------------------------------------------------------------------------------------------------------
  ggd_recovery <- bayesplot::mcmc_dens_overlay(dd_recovery_fit$post_warmup_draws) +
    dens_theme +
    labs(y = "density") +
    expand_limits(y = 0, x = 0) +
    facet_bg(fill = "lightgrey", color = NA) +
    facet_text(on = TRUE) +
    theme(aspect.ratio = 1 / 2) +
    labs(y = "Density")

  ggsave(ggd_recovery,
         dpi = 1200, device = "png",
         path = here::here("output", "parameter_recovery", "images"),
         filename = "ggd_precover.png"
         )

  #### Heatmap plot -----------------------------------------------------------------------------------------------------------
  ggh_recovery <- bayesplot::mcmc_hex(dd_recovery_fit$post_warmup_draws, bins = 50) +
    hex_theme +
    expand_limits(y = -.0, x = 0) +
    facet_bg(fill = "#E7E7E7", color = NA) +
    facet_text(on = TRUE) +
    theme(aspect.ratio = 1 / 1.5) +
    labs(x = "Delay discounting (k)", y = "Inverse temperature (beta)")

  ggsave(ggh_recovery,
       dpi = 1200, device = "png",
       path = here::here("output", "parameter_recovery", "images"),
       filename = "ggh_precover.png"
       )

} else {
  print("MODEL WAS NOT FIT, NEED TO LOAD FITTING RESULTS OR BELOW CODE WON'T EXECUTE")

  dd_par_pr <- readRDS(here::here("output", "parameter_recovery", "dd_precover.RDS"))
  dd_check_pr <- readRDS(here::here("output", "parameter_recovery", "dd_hyperbo_check_precover.RDS"))
  dd_hyperbo_loo <- readRDS(here::here("output", "parameter_recovery", "dd_hyperbo_loo_precover.RDS"))
  dd_hyperbo_parameters <- readRDS(here::here("output", "parameter_recovery", "dd_hyperbo_parameters_precover.RDS"))
}

## PARAMETER RECOVERY OUTPUTS ===============================================================================================

# examine difference between highest and lowest interval bound to quantify estimate precision
dd_par_pr %<>%
  dplyr::mutate(
    rec_precision = abs(hdi_upper_rec - hdi_lower_rec),
    rec_fit = predict(lm(value_rec ~ value, data = .)),
    resid = abs(value_rec - rec_fit)
  )

# Examine correlations between simulated and recovered parameters, both log and not log transformed
cor(dd_par_pr$value[dd_par_pr$parameter == "beta"], dd_par_pr$value_rec[dd_par_pr$parameter == "beta"])
cor(dd_par_pr$value[dd_par_pr$parameter == "k"], dd_par_pr$value_rec[dd_par_pr$parameter == "k"])
cor(dd_par_pr$value[dd_par_pr$parameter == "beta"], dd_par_pr$value_rec[dd_par_pr$parameter == "k"])
cor(dd_par_pr$value[dd_par_pr$parameter == "k"], dd_par_pr$value_rec[dd_par_pr$parameter == "beta"])


beta_values <- dd_par_pr$value[dd_par_pr$parameter == "beta"]
beta_values_rec <- dd_par_pr$value_rec[dd_par_pr$parameter == "beta"]
k_values <- dd_par_pr$value[dd_par_pr$parameter == "k"]
k_values_rec <- dd_par_pr$value_rec[dd_par_pr$parameter == "k"]

pr_cordat <- data.frame(
  beta_values = beta_values,
  beta_values_rec = beta_values_rec,
  k_values = k_values,
  k_values_rec = k_values_rec
)

# calculating the correlation matrix
cormat <- stats::cor(pr_cordat, use = "complete.obs")
colnames(cormat) <- c("β", "β(r)", "k", "k(r)")
rownames(cormat) <- c("β", "β(r)", "k", "k(r)")

# create correlation matrix
pr_corplot <- ggcorrplot::ggcorrplot(
  cormat, lab = TRUE,
  hc.order = TRUE, outline.color = "white", tl.cex = 30,
  lab_size = 9, show.legend = FALSE, colors = c("#feca8d", "#fcfdbf", "#6ece58")
  )

ggplot2::ggsave(prb_corplot,
       path = here::here("output", "parameter_recovery", "images"),
       filename = "parameter_recovery.png", dpi = 1200, device = "png"
       )

# examine spearman rank order correlations between simulated and recovered parameters, both log and not log transformed
cor(dd_par_pr$value[dd_par_pr$parameter == "k"], dd_par_pr$value_rec[dd_par_pr$parameter == "k"], method = "spearman")
cor(dd_par_pr$value[dd_par_pr$parameter == "beta"], dd_par_pr$value_rec[dd_par_pr$parameter == "beta"], method = "spearman")
cor(dd_par_pr$value[dd_par_pr$parameter == "k"], dd_par_pr$value_rec[dd_par_pr$parameter == "beta"], method = "spearman")
cor(dd_par_pr$value[dd_par_pr$parameter == "beta"], dd_par_pr$value_rec[dd_par_pr$parameter == "k"], method = "spearman")

# examine spearman rank order correlations for log(k) parameter
stats::cor(log(dd_par_pr$value[dd_par_pr$parameter == "k"]), -log(dd_par_pr$value_rec[dd_par_pr$parameter == "k"]),
  method = "spearman"
  )

#### Visualse correlation k and log(k) --------------------------------------------------------------------------------------

prec <- dd_par_pr %>%
  ggplot(., aes(x = value, y = value_rec, colour = rec_precision)) +
  geom_point(shape = 16) +
  geom_smooth(method = "lm", colour = "#2E2E2E", fill = "#A2AFB5", fullrange = TRUE, na.rm = TRUE) +
  plot_theme_legend +
  aspect_ratio_square +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .1)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  stat_poly_eq(method = "lm", label.x = .95, label.y = .95, use_label(c("eq"))) +
  labs(x = "Simualted value (k)", y = "Recovered value (k)", colour = "Imprecision") +
  scale_colour_viridis(alpha = 1)

# save plot
ggsave(prec, path = here::here("output", "parameter_recovery", "images"),
       filename = "parameter_recovery_k.png", dpi = 1200, device = "png"
       )

prec_logk <- dd_par_pr %>%
  ggplot(., aes(x = -log(value), y = -log(value_rec), colour = rec_precision)) +
  geom_point(shape = 16) +
  geom_smooth(method = "lm", colour = "#2E2E2E", fill = "#A2AFB5", fullrange = TRUE, na.rm = TRUE) +
  plot_theme_legend +
  aspect_ratio_square +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 10, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  stat_poly_eq(method = "lm", label.x = .95, label.y = .95, use_label(c("eq"))) +
  labs(x = "Simualted value (logk)", y = "Recovered value (logk)", colour = "Imprecision") +
  scale_colour_viridis(alpha = 1)

# save plot
ggsave(prec_logk, path = here::here("output", "parameter_recovery", "images"),
       filename = "parameter_recovery_logk.png", dpi = 1200, device = "png"
       )
