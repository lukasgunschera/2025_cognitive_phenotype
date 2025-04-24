## ======================================================================================================================= ##
## Script:    FUNCTION: PLOTS AND VISUALISATIONS
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Tue Apr 23 17:35:32 2024
## ======================================================================================================================= ##
## @ task_data:    Task data to generate plot for
## @ main_title:   Main title for the generated plot
## @ direction:    Plot effort by reward or reward by effort
## ======================================================================================================================= ##

## GGPLOT THEMES AND ASPECT RATIOS ==========================================================================================
# sets default design features for most plots across the project

plot_theme <- ggplot2::theme(

  # Axis formatting
  axis.line = ggplot2::element_blank(),
  axis.ticks = ggplot2::element_line(colour = "#2E2E2E", linetype = 1),
  axis.ticks.length.x = unit(0.15, "cm"),
  axis.ticks.length.y = unit(0.15, "cm"),

  # Text formatting
  text = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),
  title = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),
  axis.title = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),
  axis.text = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),



  # Margin formatting
  plot.margin = margin(.1, .1, .1, .1, "cm"),
  axis.text.y = ggplot2::element_text(margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")),
  axis.text.x = ggplot2::element_text(margin = unit(c(0.1, 0, 0, 0), "cm")),

  # Background formatting
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_rect(fill = "transparent", colour = NA_character_),
  rect = ggplot2::element_rect(fill = "transparent"),

  # Other formatting
  legend.position = "none",
)

## Generic plot theme with legend -------------------------------------------------------------------------------------------
plot_theme_legend <- ggplot2::theme(

  # Axis formatting
  axis.line = ggplot2::element_blank(),
  axis.ticks = ggplot2::element_line(colour = "#2E2E2E", linetype = 1),
  axis.ticks.length.x = unit(0.15, "cm"),
  axis.ticks.length.y = unit(0.15, "cm"),

  # Text formatting
  text = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),
  title = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),
  axis.title = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),
  axis.text = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),
  legend.title = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),
  legend.text = ggplot2::element_text(size = 12, family = "sans", colour = "#2E2E2E"),

  # Margin formatting
  plot.margin = margin(.1, .1, .1, .1, "cm"),
  axis.text.y = ggplot2::element_text(margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")),
  axis.text.x = ggplot2::element_text(margin = unit(c(0.1, 0, 0, 0), "cm")),

  # Background formatting
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_rect(fill = "transparent", colour = NA_character_),
  rect = ggplot2::element_rect(fill = "transparent"),

  # Other formatting
  legend.position = "right",
)

## Density plot theme -------------------------------------------------------------------------------------------------------
dens_theme <- ggplot2::theme(
  plot.margin = margin(.5, .5, .5, .5, "cm"),
  axis.text.y = ggplot2::element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), vjust = .5),
  axis.text.x = ggplot2::element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
  panel.grid.major.y = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  axis.line.x = ggplot2::element_line(size = .5, colour = "#2E2E2E", linetype = 1),
  axis.line.y = ggplot2::element_line(size = .5, colour = "#2E2E2E", linetype = 1),
  axis.ticks.length.x = unit(-0.1, "cm"),
  axis.ticks.length.y = unit(-0.1, "cm"),
  text = ggplot2::element_text(size = 10),
  axis.title = ggplot2::element_text(size = 10, face = "plain")
)

## hBayesDM plot themes =====================================================================================================
trace_theme <- ggplot2::theme(
  plot.margin = margin(.5, .5, .5, .5, "cm"),
  axis.text.x = ggplot2::element_text(margin = unit(c(0.15, 0, 0, 0), "cm")),
  axis.text.y = ggplot2::element_text(margin = unit(c(0, 0.15, 0, 0), "cm")),
  axis.line.x = ggplot2::element_line(size = .35, colour = "#2E2E2E", linetype = 1),
  axis.line.y = ggplot2::element_line(size = .35, colour = "#2E2E2E", linetype = 1),
  axis.ticks = ggplot2::element_line(size = .35, colour = "#2E2E2E", linetype = 1),
  axis.ticks.length.x = unit(0.15, "cm"),
  axis.ticks.length.y = unit(0.15, "cm"),

  # Text formatting
  text = ggplot2::element_text(size = 11, family = "sans", colour = "#2E2E2E"),
  title = ggplot2::element_text(size = 11),
  axis.title = ggplot2::element_text(size = 11),
  axis.text = ggplot2::element_text(size = 10),
  axis.text.x.bottom = ggplot2::element_text(vjust = -.15),
  # Background formatting
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_rect(fill = "transparent", colour = NA_character_)
)

dens_theme <- ggplot2::theme(
  plot.margin = margin(.5, .5, .5, .5, "cm"),
  axis.text.x = ggplot2::element_text(margin = unit(c(0.15, 0, 0, 0), "cm")),
  axis.text.y = ggplot2::element_text(margin = unit(c(0, 0.15, 0, 0), "cm")),
  axis.line.x = ggplot2::element_line(size = .35, colour = "#2E2E2E", linetype = 1),
  axis.ticks = ggplot2::element_line(size = .35, colour = "#2E2E2E", linetype = 1),
  axis.ticks.length.x = unit(0.15, "cm"),
  legend.position = "none",

  # Text formatting
  text = ggplot2::element_text(size = 11, family = "sans", colour = "#2E2E2E"),
  title = ggplot2::element_text(size = 11),
  axis.title = ggplot2::element_text(size = 11),
  axis.text = ggplot2::element_text(size = 10),
  axis.text.x.bottom = ggplot2::element_text(vjust = -.15),
  axis.text.y.left = ggplot2::element_blank(),
  # Background formatting
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_rect(fill = "transparent", colour = NA_character_)
)

hex_theme <- ggplot2::theme(
  plot.margin = margin(.5, .5, .5, .5, "cm"),
  axis.text.x = ggplot2::element_text(margin = unit(c(0.15, 0, 0, 0), "cm")),
  axis.text.y = ggplot2::element_text(margin = unit(c(0, 0.15, 0, 0), "cm")),
  axis.line.x = ggplot2::element_line(size = .35, colour = "#2E2E2E", linetype = 1),
  axis.line.y = ggplot2::element_line(size = .35, colour = "#2E2E2E", linetype = 1),
  axis.ticks = ggplot2::element_line(size = .35, colour = "#2E2E2E", linetype = 1),
  axis.ticks.length.x = unit(0.15, "cm"),
  axis.ticks.length.y = unit(0.15, "cm"),
  legend.position = "none",

  # Text formatting
  text = ggplot2::element_text(size = 11, family = "sans", colour = "#2E2E2E"),
  title = ggplot2::element_text(size = 11),
  axis.title = ggplot2::element_text(size = 11),
  axis.text = ggplot2::element_text(size = 10),

  # Background formatting
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_rect(fill = "transparent", colour = NA_character_)
)

## Initialise default aspect ratio options ----------------------------------------------------------------------------------
aspect_ratio_wide <- ggplot2::theme(aspect.ratio = 1 / 2)

aspect_ratio_narrow <- ggplot2::theme(aspect.ratio = 1.5 / 1)

aspect_ratio_square <- ggplot2::theme(aspect.ratio = 1 / 1)

aspect_ratio_balanced <- ggplot2::theme(aspect.ratio = 1 / 1.5)

## PLOT: HBAYESDM MODEL RESULTS =============================================================================================

create_trace_plot <- function(fit, index) {
  plot <- bayesplot::mcmc_trace(fit$post_warmup_draws) +
    trace_theme +
    aspect_ratio_balanced +
    expand_limits(y = 0, x = 0)

  ggsave(plot,
    path = here::here("output", "lcid", "dd_delaydiscount", "images", "modelfit"),
    filename = paste0("ggt", index, ".png"),
    dpi = 1200, device = "png"
  )

  return(plot)
}

# Function to create and save density plots
create_density_plot <- function(fit, index) {
  plot <- bayesplot::mcmc_dens_overlay(fit$post_warmup_draws) +
    dens_theme +
    aspect_ratio_balanced +
    labs(y = "density") +
    expand_limits(y = -.01, x = 0) +
    facet_bg(fill = "lightgrey", color = NA) +
    facet_text(on = TRUE)

  ggsave(plot,
    path = here::here("output", "lcid", "dd_delaydiscount", "images", "modelfit"),
    filename = paste0("ggd", index, ".png"),
    dpi = 1200, device = "png"
  )

  return(plot)
}

## PLOT: CHOICE FUNCTIONS ===================================================================================================

choice_plot <- function(task_dat) {
  require(ggplot2)
  require(ggpubr)
  require(dplyr)

  # Get the name of the task_dat argument
  filename <- deparse(substitute(task_dat))
  # Get wave value from filename
  wave <- as.numeric(gsub("ddtask(\\d+).*", "\\1", filename))

  plotdat <- task_dat %>%
    dplyr::mutate(difference = amount_later - amount_sooner) %>%
    dplyr::group_by(delay_later, difference) %>%
    summarise(proportion_delayed = mean(choice == 1), .groups = "drop")

  choice_plot <- plotdat %>%
    ggplot(aes(x = difference, y = 100 * proportion_delayed, colour = factor(delay_later))) +
    geom_line(size = 1) +
    labs(
      title = paste("Wave", wave),
      x = "Amount later - amount sooner",
      y = "Selection of delayed offer (%)",
      color = "Delay later (days)"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5)) +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    plot_theme +
    aspect_ratio_narrow +
    annotate(geom = "segment", x = 0, xend = 10, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
    annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = 100, colour = "#2E2E2E", lwd = 0.75) +
    scale_colour_viridis_d()

  return(choice_plot)
}

## PLOT: RAINCLOUD ==========================================================================================================
# @ dat: Data to plot
# @ title: Main title for the generated plot
# @ xlab: x axis label (when direction = horizontal this will be the y axis)
# @ ylab: y axis label (when direction = horizontal this will be the x axis)
# @ predictor_var: name of predictor variable
# @ outcome_var: name of outcome variable
# @ predictor_tick_lab: tick labels for predictor variable
# @ col: color(s) to use
# @ direction: plot horizontally or vertically?
# @ include_grouping: plot grouping variable?
# @ group_var: (if include_grouping is TRUE) name of grouping variable
# @ legendlab: (if include_grouping is TRUE) title of group legend

raincloud_plot <- function(dat, title, xlab, ylab,
                           predictor_var, outcome_var,
                           predictor_tick_lab, col, direction = "vertical",
                           include_grouping = FALSE, group_var,
                           legendlab = "", scale_seq = c(0, 6, 2)) {
  require(ggplot2)
  require(PupillometryR)

  if (include_grouping == FALSE) {
    rain_plot <- ggplot(
      data = dat,
      aes_string(y = outcome_var, x = predictor_var, fill = predictor_var)
    ) +
      geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.6) +
      geom_point(aes_string(y = outcome_var, color = predictor_var),
        position = position_jitter(width = 0.15),
        size = 0.75, alpha = 0.6
      ) +
      geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
      guides(fill = "none", color = "none") +
      labs(title = title, x = xlab, y = ylab)
  } else {
    rain_plot <- ggplot(
      data = dat,
      aes_string(y = outcome_var, x = predictor_var, fill = group_var)
    ) +
      geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.6) +
      geom_point(aes_string(y = outcome_var, color = group_var),
        position = position_jitter(width = 0.15),
        size = 0.75, alpha = 0.6
      ) +
      geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
      guides(color = guide_legend(override.aes = list(size = 10)), fill = "none") +
      labs(title = title, x = xlab, y = ylab, color = legendlab)
  }
  rain_plot <- rain_plot +
    scale_fill_viridis_d() +
    scale_colour_viridis_d() +
    scale_x_discrete(expand = c(0.1, 0.1), labels = predictor_tick_lab) +
    scale_y_continuous(
      breaks = seq(scale_seq[1], scale_seq[2], scale_seq[3]),
      limits = c(scale_seq[1], scale_seq[2])
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "top"
    )

  if (direction == "horizontal") {
    rain_plot <- rain_plot + coord_flip()
  }
  rain_plot
}

## PLOT: PCUMULATIVE LAGS ===================================================================================================

cum_plot <- function(dat, var_labels, title,
                     x_label, y_label, y_lim, col, shape = 1) {
  cum_plot <- ggplot(dat, aes(
    x = trial, y = mean_value,
    group = variable, color = variable
  )) +
    geom_line() +
    geom_point(size = 0.75, shape = shape) +
    geom_ribbon(
      aes(
        ymin = se_lower,
        ymax = se_upper,
        x = trial, group = variable, color = variable,
        fill = variable
      ),
      outline.type = "both", alpha = 0.25
    ) +
    scale_fill_manual(
      values = col, name = " ",
      labels = var_labels
    ) +
    scale_color_manual(
      values = col, name = " ",
      labels = var_labels
    ) +
    labs(
      title = title,
      x = x_label, y = y_label
    ) +
    ylim(y_lim) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 8, hjust = 0.5, face = "bold"),
      axis.title = ggplot2::element_text(size = 6),
      axis.text.x = ggplot2::element_text(size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 8)
    )

  cum_plot
}

## PLOT: POSTERIOR PREDICTIVE CHECKS ========================================================================================
# @ ppc_dat: PPC data to visualize (output from "posterior_predictions" function)
# @ group_plot: Group level plot (compares observed and predicted group-level
# acceptance proportions per effort and reward level)?
# @ indiv_plot: Individual level plot (observed vs. predicted acceptance
# proportions on a subject level for each effort and reward level)?

ppc_plots <- function(ppc_dat,
                      group_plot = TRUE,
                      indiv_plot = TRUE,
                      group_plot_title = "Observed vs. model predicted acceptance proportions",
                      indiv_plot_title = "Subject wise observed vs. model predicted acceptance proportions ") {
  require(ggplot2)
  require(ggpubr)

  amount <- sort(unique(ppc_dat$posterior_predictions_trial_type$amount_later))

  res_plots <- list()

  # Plot 1 - group wise
  if (group_plot == TRUE) {
    obs_mean <- aggregate(
      ppc_dat$posterior_predictions_trial_type$observation ~
        ppc_dat$posterior_predictions_trial_type$delay_later +
        ppc_dat$posterior_predictions_trial_type$amount_later,
      FUN = function(x) {
        mean(x) * 100
      }
    )
    obs_se <- aggregate(
      ppc_dat$posterior_predictions_trial_type$observation ~
        ppc_dat$posterior_predictions_trial_type$delay_later +
        ppc_dat$posterior_predictions_trial_type$amount_later,
      FUN = function(x) {
        (sd(x) / sqrt(length(x))) * 100
      }
    )

    observed <- cbind(obs_mean, obs_se[, 3])
    colnames(observed) <- c("delay_later", "amount_later", "mean", "se")
    observed$effort_a <- as.factor(observed$delay_later)
    observed$amount_a <- as.factor(observed$amount_later)

    pred_mean <- aggregate(
      ppc_dat$posterior_predictions_trial_type$prediction_mean ~
        ppc_dat$posterior_predictions_trial_type$delay_later +
        ppc_dat$posterior_predictions_trial_type$amount_later,
      FUN = function(x) {
        mean(x) * 100
      }
    )
    pred_se <- aggregate(
      ppc_dat$posterior_predictions_trial_type$prediction_mean ~
        ppc_dat$posterior_predictions_trial_type$delay_later +
        ppc_dat$posterior_predictions_trial_type$amount_later,
      FUN = function(x) {
        (sd(x) / sqrt(length(x))) * 100
      }
    )
    predicted <- cbind(pred_mean, pred_se[, 3])
    colnames(predicted) <- c("delay_later", "amount_later", "mean", "se")
    predicted$delay_later <- as.factor(predicted$delay_later)
    predicted$amount_later <- as.factor(predicted$amount_later)

    group_plot_dat <- rbind(observed, predicted)
    group_plot_dat <- cbind(group_plot_dat, "group" = paste(rep(c("pred", "real"), each = 16),
      rep(1:4, 4), rep(1:4, each = 4),
      sep = ""
    ))

    group_plot <- list()

    for (i in 1:4) {
      group_plot[[i]] <- ggplot(group_plot_dat[group_plot_dat$amount_later == amount[i], ], aes(x = delay_later, y = mean, fill = group)) +
        geom_bar(stat = "identity", position = position_dodge(), alpha = 0.9) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
          width = .3,
          position = position_dodge(0.95)
        ) +
        scale_fill_viridis_d(labels = c(
          "Predicted", "Predicted", "Predicted", "Predicted",
          "Observed", "Observed", "Observed", "Observed"
        )) +
        ylab("% Accepted") +
        xlab("Effort level") +
        ggtitle(paste("Reward Level", i)) +
        guides(fill = guide_legend(title = " ")) +
        scale_x_discrete(labels = 1:4) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 8),
          axis.title = ggplot2::element_text(size = 8),
          axis.text.x = ggplot2::element_text(size = 8),
          axis.text.y = ggplot2::element_text(size = 8),
          legend.title = ggplot2::element_text(size = 8),
          legend.text = ggplot2::element_text(size = 8)
        )
    }

    group_plot <- ggarrange(group_plot[[1]],
      group_plot[[2]],
      group_plot[[3]],
      group_plot[[4]],
      ncol = 2, nrow = 2,
      common.legend = TRUE,
      legend = "bottom"
    )

    group_plot <- annotate_figure(group_plot,
      top = text_grob(group_plot_title,
        face = "bold", size = 8
      )
    )

    group_plot

    res_plots[["group_plot"]] <- group_plot
  }

  # Plot 2 - subject wise
  if (indiv_plot == TRUE) {
    # plot by effort level
    indiv_plot_delay_dat <- ppc_dat$posterior_predictions_delay
    indiv_plot_delay_dat$delay_later <- as.factor(indiv_plot_effort_dat$delay_later)

    R_squared_effort <- round(cor(indiv_plot_delay_dat$observation, indiv_plot_delay_dat$prediction_mean)^2, 2)

    indiv_plot_delay <- ggplot(indiv_plot_delay_dat, aes(x = observation, y = prediction_mean, color = delay_later)) +
      geom_point(size = 2, alpha = 0.25) +
      geom_errorbar(aes(ymin = prediction_hdi_lower, ymax = prediction_hdi_higher), width = .025, alpha = 0.1) +
      scale_color_manual(
        values = c("#E94D36", "#5B9BD5", "#71AB48", "#FFBF00"),
        labels = 1:4
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      geom_abline(linetype = 3) +
      ylab("Predicted (± 95% HDI)") +
      xlab("Observed") +
      ggtitle(bquote("Across effort levels:" ~ R^{
        2
      } == .(R_squared_delay))) +
      guides(color = guide_legend(title = "Delay/Reward level")) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 10),
        legend.text = ggplot2::element_text(size = 10)
      )

    # plot by reward level
    indiv_plot_amount_dat <- ppc_dat$posterior_predictions_amount
    indiv_plot_amount_dat$amount_later <- as.factor(indiv_plot_amount_dat$amount_later)

    R_squared_effort <- round(cor(indiv_plot_amount_dat$observation, indiv_plot_amount_dat$prediction_mean)^2, 2)

    indiv_plot_amount <- ggplot(indiv_plot_amount_dat, aes(x = observation, y = prediction_mean, color = amount_later)) +
      geom_point(size = 2, alpha = 0.25) +
      geom_errorbar(aes(ymin = prediction_hdi_lower, ymax = prediction_hdi_higher), width = .025, alpha = 0.1) +
      scale_color_manual(
        values = c("#E94D36", "#5B9BD5", "#71AB48", "#FFBF00"),
        labels = 1:4
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      geom_abline(linetype = 3) +
      ylab("Predicted (± 95% HDI)") +
      xlab("Observed") +
      ggtitle(bquote("Across reward levels:" ~ R^{
        2
      } == .(R_squared_delay))) +
      guides(color = guide_legend(title = "Delay/Reward level")) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 10),
        legend.text = ggplot2::element_text(size = 10)
      )


    indiv_plot <- ggarrange(indiv_plot_delay,
      indiv_plot_amount,
      ncol = 2, nrow = 1,
      common.legend = TRUE,
      legend = "bottom"
    )

    indiv_plot <- annotate_figure(indiv_plot,
      top = text_grob(indiv_plot_title,
        face = "bold", size = 10
      )
    )


    res_plots[["indiv_plot"]] <- indiv_plot
  }
  return(res_plots)
}

## POSTERIOR PREDICTIVE CORRELATIONS =========================================================================================
# @ ppc_data: tibble with posterior predictive accuracy averaged for trial types (delays)

ppc_plot <- function(ppc_data, jitter_amount = 0.05) {
  # Custom jitter function that respects bounds
  bounded_jitter <- function(x, amount) {
    # For each point, calculate how much we can jitter up or down
    # Points near 1 can only jitter down, points near 0 can only jitter up
    jitter_values <- sapply(x, function(val) {
      if (val >= 0.5) {
        # For points above 0.5, primarily jitter downward
        max_up <- amount * (1 - val) # reduces jitter as we approach 1
        max_down <- amount * val # allows more downward jitter
        runif(1, min = -max_down, max = max_up)
      } else {
        # For points below 0.5, primarily jitter upward
        max_up <- amount * (1 - val) # allows more upward jitter
        max_down <- amount * val # reduces jitter as we approach 0
        runif(1, min = -max_down, max = max_up)
      }
    })
    return(x + jitter_values)
  }

  ppc_data %>%
    ggplot(aes(
      x = mean_pred_choice,
      y = bounded_jitter(mean_real_choice, jitter_amount)
    )) +
    geom_point(
      shape = 20,
      aes(colour = abs(hdi_upper_pred_choice - hdi_lower_pred_choice)),
      position = position_jitter(width = jitter_amount),
      size = 2,
      alpha = .75
    ) +
    geom_smooth(
      method = "lm",
      color = "#2E2E2E",
      fullrange = TRUE
    ) +
    stat_poly_eq(use_label(c("R2")),
      label.y = 0.85,
      size = 4,
      colour = "#2E2E2E",
      parse = TRUE,
      position = "identity"
    ) +
    plot_theme +
    aspect_ratio_square +
    scale_colour_viridis(
      name = "Imprecision",
      direction = 1,
      limits = c(0, 1),
      breaks = seq(0, 1, .5)
    ) +
    scale_x_continuous(
      breaks = seq(0, 1, .5),
      expand = c(0.025, 0.025),
      limits = c(0, 1)
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, .5),
      expand = c(0.05, 0.05),
      limits = c(0, 1)
    ) +
    theme(
      legend.title = element_text(margin = margin(b = 10, r = 5), size = 10),
      legend.text = element_text(size = 10),
      text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10)
    ) +
    labs(x = "Predicted choice", y = "Observed choice") +
    annotate(
      x = -Inf, xend = -Inf, y = 0, yend = 1,
      colour = "#2E2E2E", lwd = 0.75, geom = "segment"
    ) +
    annotate(
      x = 0, xend = 1, y = -Inf, yend = -Inf,
      colour = "#2E2E2E", lwd = 0.75, geom = "segment"
    )
}

## PARAMETER RECOVERY =======================================================================================================
# @ recovery_data: tibble with underlying and recovered parameter values
# @ plot_title: title for plot
# @ col: color to use

params_recovery_plot <- function(recovery_data,
                                 plot_title,
                                 col) {
  params_recovery_plot <- ggplot(
    recovery_data,
    aes(
      x = real, y = recovered,
      color = col
    )
  ) +
    geom_point(size = 2, alpha = 0.5, color = col) +
    geom_abline(linetype = 3) +
    ylab("Recovered parameter estimates") +
    xlab("Underlying parameters") +
    labs(title = plot_title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10),
      plot.subtitle = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "none"
    )

  return(params_recovery_plot)
}

## DROP LEADING ZEROES FOR LABELS ===========================================================================================

drop_leading_zeros <- function(l) {
  stringr::str_replace(l, "0(?=.)", "")
}
