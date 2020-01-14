
# package setup -----------------------------------------------------------
checkpoint::checkpoint("2019-10-01", scanForPackages = FALSE)

library(SimDesign)
library(moments)
library(tidyverse)
library(furrr)
library(ggnewscale)
library(RcppEigen)


# custom functions --------------------------------------------------------

find_x <- function(fit, y, ...){
  newdata_u <- substitute(list(...))
  uniroot(function(x) {predict(fit, eval(newdata_u)) - y}, interval = c(0, 1))$root
}


do_simulation <- function(b12, b3, ...) {

  sigma <- matrix(c(
    1, 3/7, 3/7, 1
  ), nrow = 2)

  x <- rValeMaurelli(1e4, sigma = sigma, skew = rep(2, 2), kurt = rep(7, 2))
  x <- cbind(1, x, x[, 1] * x[, 2])

  b <- c(0, b12, b12, b3)

  y <- x %*% b
  y <- y + rnorm(nrow(x), sd = sqrt(1 - var(y)))

  fit_0 <- fastLmPure(x[, -4], y)
  r2_0  <- cor(y, fit_0$fitted.values)^2

  fit_1 <- fastLmPure(x, y)
  r2_1  <- cor(y, fit_1$fitted.values)^2

  list(
    r2_0 = r2_0,
    r2_1 = r2_1,
    var_y = var(y),
    skew = skewness(x),
    kurt = kurtosis(x),
    dat = cbind(x, y)
  )
}



# simulation --------------------------------------------------------------

dsn <- crossing(
  b12 = seq(.2190, .3000, by = .0025),
  b3 = seq(.13, .30, by = .0025),
  rep = 1:5
)

plan(multiprocess, workers = 4L)

results <- future_pmap(
  dsn, do_simulation,
  .progress = TRUE,
  .options = future_options(seed = 12479L)
) %>%
  transpose() %>%
  as_tibble() %>%
  mutate_at(vars(r2_0:var_y), as.numeric) %>%
  mutate(r2change = r2_1 - r2_0) %>%
  bind_cols(dsn, .)




# analyses ----------------------------------------------------------------

# preliminary plots
results <- results %>% sample_frac()

ggplot(data = results, aes(x = b3, y = r2change, color = b12)) +
  geom_jitter(width = .005) +
  geom_hline(yintercept = c(.035, .1)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red")

ggplot(data = results, aes(x = b12, y = r2_0, color = b3)) +
  geom_jitter(width = .005) +
  geom_hline(yintercept = .3)



# find values
fit <- lm(r2change ~ poly(b3, 2), data = results)
fit2 <- lm(r2_0 ~ b12 + b3, data = results)

b3_1   <- find_x(fit, .1, b3 = x)
b3_035 <- find_x(fit, .035, b3 = x)

b12_1 <- find_x(fit2, .30, b12 = x, b3 = b3_1)
b12_035 <- find_x(fit2, .30, b12 = x, b3 = b3_035)


# print results
rbind(b12_035, b3_035, b12_1, b3_1)

ugh <- tribble(
  ~ group, ~ b3,
  0, b3_035,
  1, b3_1
) %>%
  crossing(b12 = c(.22, .30)) %>%
  mutate(r2_0 = predict(fit2, newdata = .))



ggplot(mapping = aes(x = b12, y = r2_0)) +
  geom_jitter(data = results, aes(color = b3), width = .005) +
  scale_color_gradient() +
  geom_hline(yintercept = .3) +
  new_scale_color() +
  geom_line(aes(x = b12, y = r2_0, group = group, color = factor(b3)), data = ugh, size = 2, inherit.aes = FALSE)

ggPredict(fit2)
