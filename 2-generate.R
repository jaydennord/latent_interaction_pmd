#!/usr/bin/env Rscript

n_reps <- commandArgs(trailingOnly=TRUE) # 1000


# packages ----------------------------------------------------------------

checkpoint::checkpoint("2019-10-01", scanForPackages = FALSE)
library(data.table)
library(SimDesign)
library(moments)
library(tidyverse)
library(stringi)

# functions ---------------------------------------------------------------

gen_data <- function(n, skew, kurt, b12, b3, resid, meas_var, cor, missing, data_path, ...) {

  sigma <- matrix(c(1, cor, cor, 1), nrow = 2)

  lambda <- sqrt(meas_var)
  meas_err <- 1 - meas_var

  lambda_mat <- sqrt(matrix(c(
    1, lambda, lambda, lambda, 0,  0,  0,  0, 0,  0,  0,  0,
    0,  0,  0,  0, 1, lambda, lambda, lambda, 0,  0,  0,  0,
    0,  0,  0,  0, 0,  0,  0,  0, 1, lambda, lambda, lambda
  ), nrow = 3, byrow = TRUE))

  y_error <- c(1 * meas_err / meas_var, rep(1 - meas_var, times = 3))

  xi <- SimDesign::rValeMaurelli(
    n, sigma = sigma,
    skew = rep(skew, 2),
    kurt = rep(kurt, 2)
  )

  interaction <- xi[, 1] * xi[, 2]

  eta <- cbind(xi, interaction) %*% c(b12, b12, b3) + rnorm(n, sd = sqrt(resid))

  lat <- cbind(xi, eta)

  y <- 8 + cbind(xi, eta) %*% lambda_mat +
    sapply(rep(sqrt(y_error), 3), rnorm, n = n, mean = 0)

  if (missing == "pmd") {
    assignment <- sample(1:nrow(pmd_base), size = n, replace = TRUE)
    keep <- pmd_base[assignment, ]
    y[!keep] <- NA_real_
  }

  y_centered <- scale(y, center = TRUE, scale = FALSE)

  y_prod <- y_centered[, 1:4] * y_centered[, 5:8]

  y_prod_centered <- scale(y_prod, scale = FALSE)

  d <- as.data.frame(cbind(y, y_centered, y_prod_centered))

  # side effect
  data.table::fwrite(d, data_path, sep = ",", na = "-999", col.names = FALSE)

  # # return
  list(
    skew = skewness(lat),
    kurt = kurtosis(lat),
    mean = colMeans(lat),
    cov  = cov(lat)
  )


}




# constants ---------------------------------------------------------------


dirs <- c(
  "data",
  "inpout"
)

for(dir in dirs) if (!dir.exists(dir)) dir.create(dir)

# lambda <- sqrt(matrix(c(
#   1, .7, .7, .7, 0,  0,  0,  0, 0,  0,  0,  0,
#   0,  0,  0,  0, 1, .7, .7, .7, 0,  0,  0,  0,
#   0,  0,  0,  0, 0,  0,  0,  0, 1, .7, .7, .7
# ), nrow = 3, byrow = TRUE))

pmd_base <- matrix(c(
  T, T, T,
  T, T, F,
  T, F, T,
  F, T, T
), nrow = 3, ncol = 12)

# mplus_template <- '
# DATA:
# FILE = .<csv>;
#
# VARIABLE:
# NAMES = x1 x2 x3 x4 z1 z2 z3 z4 y1 y2 y3 y4
# x1c x2c x3c x4c z1c z2c z3c z4c y1c y2c y3c y4c
# x1cz1cc x2cz2cc x3cz3cc x4cz4cc;
# MISSING = ALL(-999);
# USEVARIABLES = <use_vars>;
#
# ANALYSIS:
# TYPE = <type>;
# ESTIMATOR = <estimator>;
# PROCESSORS = 1;
#
# MODEL:
# <model>
#
# y ON x z xz;
#
# SAVEDATA:
# RESULTS = <res>;
# '

dmc_template <- '
DATA:
  FILE = .<csv>;

VARIABLE:
  NAMES = x1 x2 x3 x4 z1 z2 z3 z4 y1 y2 y3 y4
    x1c x2c x3c x4c z1c z2c z3c z4c y1c y2c y3c y4c
    x1cz1cc x2cz2cc x3cz3cc x4cz4cc;
  MISSING = ALL(-999);
  USEVARIABLES = x1c-x4cz4cc;

ANALYSIS:
  TYPE = GENERAL;
  ESTIMATOR = <estimator>;
  PROCESSORS = 1;

MODEL:
  x BY x1c-x4c;
  z BY z1c-z4c;
  y BY y1c-y4c;
  xz BY x1cz1cc-x4cz4cc;
  y ON x z xz;

  x WITH z* xz*;
  z WITH xz*;
  [x1c-y4c@0 x1cz1cc-x4cz4cc@0];
'

lms_template <- '
DATA:
  FILE = .<csv>;

VARIABLE:
  NAMES = x1 x2 x3 x4 z1 z2 z3 z4 y1 y2 y3 y4
    x1c x2c x3c x4c z1c z2c z3c z4c y1c y2c y3c y4c
    x1cz1cc x2cz2cc x3cz3cc x4cz4cc;
  MISSING = ALL(-999);
  USEVARIABLES = x1-y4;

ANALYSIS:
  TYPE = RANDOM;
  METHOD = INTEGRATION;
  ESTIMATOR = <estimator>;
  PROCESSORS = 1;

MODEL:
  x BY x1-x4;
  z BY z1-z4;
  y BY y1-y4;
  xz | x XWITH z
  y ON x z xz;
'



# simulation --------------------------------------------------------------

# indicator loadings?
# predictor correlation? hell no

set.seed(12479)

dsn <- crossing(
  shape = c("norm", "nonnorm"),  # could add mid-level non-normal condition, or just another one
  # int = c("no", "me", "hi"),
  int = c(0, .035, .1),
  cor = c(.1, .3, .5),
  meas_var = c(.3, .7),
  n = c(200, 375, 550),            # add big big sample size
  missing = c("complete", "pmd"),
  rep = str_pad(1:n_reps, pad = "0", width = 5)
) %>%
  mutate(
    gen_id = stringi::stri_rand_strings(n(), length = 10),
    sim_block = sample(0:749, size = n(), replace = TRUE),
    data_path = paste0("./data/", gen_id, ".csv")
  )


shape <- tribble(
  ~shape, ~skew, ~kurt,
  "norm", 0, 0,
  "nonnorm", 2, 7
)

# coef <- tribble(
#   ~shape, ~int, ~g12, ~g3, ~var,
#    "normal",  "no", .3240370, .0000000, .700,
#    "normal",  "me", .3240370, .1719563, .665,
#    "normal",  "hi", .3240370, .2906592, .600,
#    "nonnorm", "no", .3240370, .0000000, .700,
#    "nonnorm", "me", .2692575, .1363365, .665,
#    "nonnorm", "hi", .2278120, .2304313, .600
# )

coef <- tribble(
  ~cor, ~int, ~shape, ~b12, ~b3, ~resid,
  0.1, 0.000, "norm", 0.3694841, 0.0000000, 0.700,
  0.3, 0.000, "norm", 0.3391303, 0.0000000, 0.700,
  0.5, 0.000, "norm", 0.3163527, 0.0000000, 0.700,
  0.1, 0.035, "norm", 0.3691284, 0.1853128, 0.665,
  0.3, 0.035, "norm", 0.3392449, 0.1795406, 0.665,
  0.5, 0.035, "norm", 0.3167031, 0.1675587, 0.665,
  0.1, 0.100, "norm", 0.3690739, 0.3144915, 0.600,
  0.3, 0.100, "norm", 0.3394487, 0.3028050, 0.600,
  0.5, 0.100, "norm", 0.3156257, 0.2828707, 0.600,
  0.1, 0.000, "nonnorm", 0.3712515, 0.0000000, 0.700,
  0.3, 0.000, "nonnorm", 0.3445684, 0.0000000, 0.700,
  0.5, 0.000, "nonnorm", 0.3206627, 0.0000000, 0.700,
  0.1, 0.035, "nonnorm", 0.3537166, 0.1761638, 0.665,
  0.3, 0.035, "nonnorm", 0.2991248, 0.1530796, 0.665,
  0.5, 0.035, "nonnorm", 0.2551544, 0.1306651, 0.665,
  0.1, 0.100, "nonnorm", 0.3420580, 0.2990683, 0.600,
  0.3, 0.100, "nonnorm", 0.2664815, 0.2580367, 0.600,
  0.5, 0.100, "nonnorm", 0.2109073, 0.2196709, 0.600
)



dsn <- list(dsn, shape, coef) %>%
  reduce(left_join)


dsn2 <- crossing(
  dsn,
  method = c("LMS", "DMC"),
  estimator = c("ML", "MLR")  # could add bootstrap and/or bayes estimator... but anticipate poor completion times
) %>%
  mutate(
    # cond = paste(cond, method, estimator, sep = "_"),
    ana_id = stri_rand_strings(n(), 10),
    inp = paste0("./inpout/", ana_id, ".inp"),
    out = paste0("./inpout/", ana_id, ".out"),
    res = paste0("./inpout/", ana_id, ".res"),

    template = if_else(method == "LMS", lms_template, dmc_template),

    inp_string = pmap_chr(list(template, data_path, estimator), ~{

      stri_replace_all_regex(
        ..1,
        c("<csv>", "<estimator>"),
        c(..2, ..3),
        vectorize_all = FALSE
      )

    })
    )

dsn %>% fwrite("dsn.csv", sep = ",")
dsn2 %>% select(-inp_string, -template) %>% fwrite("dsn2.csv", sep = ",")

dcmd <- transmute(
  .data = dsn2,
  sim_block,
  cmd = paste0("~/mplus ", basename(inp))
) %>%
  nest(cmd)

pwalk(dcmd, ~ {

  # out <- c("cd inpout", ..2$cmd)

  cat(
    "cd inpout", ..2$cmd,
    file = paste0("slurm/slurm", ..1),
    sep = "\n"
  )

})

#set.seed(12479)
#pwalk(dsn, gen_data)
#walk2(dsn3$inp_string, dsn3$inp, write_file)
