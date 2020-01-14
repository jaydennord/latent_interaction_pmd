checkpoint::checkpoint("2019-10-01", scanForPackages = FALSE)
library(tidyverse)
library(latex2exp)


d <- read_csv("final_summaries.csv") %>%
  mutate(
    x = paste(method, estimator),
    n_missing = interaction(n, missing),
    shape = factor(shape, levels = c("norm", "nonnorm"))
  )

theme_nord <- theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, vjust = .5),
    axis.ticks.y = element_line(color = "black")
  )

theme_set(theme_nord)

bias <- ggplot(
  data = filter(d, int == 0),
  aes(
    x = x,
    y = bias,
    ymin = bias_lo,
    ymax = bias_hi)
) +
  geom_point(aes(shape = n_missing), position = position_dodge(.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1, 2, 0)) +
  geom_errorbar(aes(linetype = n_missing), position = position_dodge(.5)) +
  facet_grid(shape + int ~ meas_var + cor, labeller = labeller(
    shape = c("norm" = "Normal", "nonnorm" = "Non-normal"),
    int = as_labeller(function(s) TeX(paste0("$\\Delta\\mathit{R}^2 = ", s, "$")), default = label_parsed),
    meas_var = as_labeller(function(s) TeX(paste0("$\\sigma^2 = ", s, "$")), default = label_parsed),
    cor = as_labeller(function(s) TeX(paste0("$\\mathit{\\rho} = ", s, "$")), default = label_parsed),
    .default = label_parsed
  ), drop = TRUE) +
labs(
  shape = "Sample size &\n Missingness",
  linetype = "Sample size &\n Missingness",
  x = NULL,
  y = "Bias"
)


std_bias <- ggplot(
  data = filter(d, int != 0, estimator == "ML"),
  aes(
    x = method,
    y = std_bias,
    ymin = std_bias_lo,
    ymax = std_bias_hi)
) +
  scale_x_continuous() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = .05, ymax = Inf, fill = "gray", alpha = .5) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -.05, fill = "gray", alpha = .5) +
  scale_x_discrete() +
  geom_point(aes(shape = n_missing), position = position_dodge(.75)) +
  scale_shape_manual(values = c(16, 17, 15, 1, 2, 0)) +
  geom_errorbar(aes(linetype = n_missing), position = position_dodge(.75)) +
  facet_grid(shape + int~ meas_var + cor, labeller = labeller(
    shape = c("norm" = "Normal", "nonnorm" = "Non-normal"),
    int = as_labeller(function(s) TeX(paste0("$\\Delta\\mathit{R}^2 = ", s, "$")), default = label_parsed),
    meas_var = as_labeller(function(s) TeX(paste0("$\\sigma^2 = ", s, "$")), default = label_parsed),
    cor = as_labeller(function(s) TeX(paste0("$\\mathit{\\rho} = ", s, "$")), default = label_parsed),
    .default = label_parsed
  ), drop = TRUE) +
labs(
  shape = "Sample size &\n Missingness",
  linetype = "Sample size &\n Missingness",
  x = NULL,
  y = "Relative bias"
); std_bias



# update data
d <- d %>% filter(
  !(shape == "nonnorm" & meas_var == .3)
)





se_bias <- ggplot(
  data = d,
  aes(
    x = x,
    y = se_std_bias,
    ymin = se_std_lo,
    ymax = se_std_hi
    )
  ) +
  scale_x_continuous() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = .05, ymax = Inf, fill = "gray", alpha = .5) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -.05, fill = "gray", alpha = .5) +
  scale_x_discrete() +
  geom_point(aes(shape = n_missing), position = position_dodge(.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1, 2, 0)) +
  geom_errorbar(aes(linetype = n_missing), position = position_dodge(.5)) +
  facet_grid(shape + int~ meas_var + cor, labeller = labeller(
    shape = c("norm" = "Normal", "nonnorm" = "Non-normal"),
    int = as_labeller(function(s) TeX(paste0("$\\Delta\\mathit{R}^2 = ", s, "$")), default = label_parsed),
    meas_var = as_labeller(function(s) TeX(paste0("$\\sigma^2 = ", s, "$")), default = label_parsed),
    cor = as_labeller(function(s) TeX(paste0("$\\mathit{\\rho} = ", s, "$")), default = label_parsed),
    .default = label_parsed
  ), drop = TRUE) +
  labs(
    shape = "Sample size &\n Missingness",
    linetype = "Sample size &\n Missingness",
    x = NULL,
    y = "Relative SE bias"
  ); se_bias





d <- d %>% filter(
  !(shape == "nonnorm" & method == "DMC")
)

flub <- d %>% nest(
  -c(shape, int, cor, meas_var)
) %>%
  crossing(
    key = c("lo", "hi")
  ) %>%
  mutate(
    data = NULL,
    yintercept = if_else(int == 0, .05, .80),
    y = case_when(
      key == "lo" & int == 0 ~ 0,
      key == "hi" & int == 0 ~ .15,

      key == "lo" & int != 0 ~ .55,
      key == "hi" & int != 0 ~ 1
    )

  )



rejection <- ggplot(
  data = d,
  aes(
    x = x,
    y = rejection,
    ymin = rejection_lo,
    ymax = rejection_hi)
) +
  geom_hline(data = flub, aes(yintercept = yintercept), alpha = .4, size = 1.005) +
  geom_point(aes(shape = n_missing), position = position_dodge(.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1, 2, 0)) +
  geom_errorbar(aes(linetype = n_missing), position = position_dodge(.5)) +
  geom_blank(data = flub, aes(x=0, y = y), inherit.aes = FALSE) +
  facet_grid(shape + int~ meas_var + cor, scales = "free_y", labeller = labeller(
    shape = c("norm" = "Normal", "nonnorm" = "Non-normal"),
    int = as_labeller(function(s) TeX(paste0("$\\Delta\\mathit{R}^2 = ", s, "$")), default = label_parsed),
    meas_var = as_labeller(function(s) TeX(paste0("$\\sigma^2 = ", s, "$")), default = label_parsed),
    cor = as_labeller(function(s) TeX(paste0("$\\mathit{\\rho} = ", s, "$")), default = label_parsed),
    .default = label_parsed
  )) +
labs(
  shape = "Sample size &\n Missingness",
  linetype = "Sample size &\n Missingness",
  x = NULL,
  y = expression("Rejection of "~gamma[3]==0)
); rejection

d <- d %>% filter(
  shape != "nonnorm"
)

coverage <- ggplot(
  data = d,
  aes(
    x = x,
    y = coverage,
    ymin = coverage_lo,
    ymax = coverage_hi)
) +
  geom_hline(yintercept = .95, alpha = .4) +
  geom_point(aes(shape = n_missing), position = position_dodge(.5)) +
  scale_shape_manual(values = c(16, 17, 15, 1, 2, 0)) +
  geom_errorbar(aes(linetype = n_missing), position = position_dodge(.5)) +
  facet_grid(shape + int~ meas_var + cor, labeller = as_labeller(TeX, default = label_parsed)) +
  facet_grid(shape + int~ meas_var + cor, scales = "free_y", labeller = labeller(
    shape = c("norm" = "Normal", "nonnorm" = "Non-normal"),
    int = as_labeller(function(s) TeX(paste0("$\\Delta\\mathit{R}^2 = ", s, "$")), default = label_parsed),
    meas_var = as_labeller(function(s) TeX(paste0("$\\sigma^2 = ", s, "$")), default = label_parsed),
    cor = as_labeller(function(s) TeX(paste0("$\\mathit{\\rho} = ", s, "$")), default = label_parsed),
    .default = label_parsed
  )) +
  labs(
    shape = "Sample size &\n Missingness",
    linetype = "Sample size &\n Missingness",
    x = NULL,
    y = "Coverage"
  ); coverage


re <- ggplot(
  data = filter(d, missing == "complete"),
  aes(
    x = x,
    y = re,
    ymin = re_lo,
    ymax = re_hi)
) +
  geom_point(aes(shape = factor(n)), position = position_dodge(.5)) +
  geom_errorbar(aes(linetype = factor(n)), position = position_dodge(.5)) +
  facet_grid(shape + int ~ meas_var + cor, labeller = as_labeller(TeX, default = label_parsed)) +
  labs(
    shape = "Sample size",
    linetype = "Sample size",
    x = NULL,
    y = "Relative efficiency"
  ) ; re


ggsave(filename = "plot_1_bias.png", bias, width = 6.5, height = 6.5, dpi = 300, scale = 1.5)
ggsave(filename = "plot_2_std_bias.png", std_bias, width = 6.5, height = 6.5, dpi = 300, scale = 1.5)
ggsave(filename = "plot_3_se_bias.png", se_bias, width = 6.5, height = 6.5, dpi = 300, scale = 1.5)
ggsave(filename = "plot_4_coverage.png", coverage, width = 6.5, height = 6.5, dpi = 300, scale = 1.5)
ggsave(filename = "plot_5_rejection.png", rejection, width = 6.5, height = 6.5, dpi = 300, scale = 1.5)
