checkpoint::checkpoint("2019-10-01", scanForPackages = FALSE)
library(data.table)
library(binom)




se_bias_boot <- function(x, y, r = 5, ...) {

  i_list <- replicate(r, sample(length(x), replace = TRUE), simplify = FALSE)

  booted <- vapply(i_list, function(i) {
    mean((x[i] - sd(y[i])) / sd(y[i]))
  }, numeric(1))

  out <- as.list(quantile(booted, c (.025, .975), na.rm = TRUE))
  names(out) <- c("se_lo", "se_hi")
  out

}

re_boot <- function(est_com, est_pmd, r = 5, ...) {

  i_list <- replicate(r, sample(length(est_com), replace = TRUE), simplify = FALSE)

  booted <- vapply(i_list, function(i) {
    var(est_com[i], na.rm = TRUE) / var(est_pmd[i], na.rm = TRUE)
  }, numeric(1))

  list(
    re = var(est_com, na.rm = TRUE) / var(est_pmd, na.rm = TRUE),
    re_lo = quantile(booted, .025, na.rm = TRUE),
    re_hi = quantile(booted, .975, na.rm = TRUE)
  )

}

d <- fread("final_results.csv", sep = ",")

grps <- c("shape", "int", "cor", "meas_var", "n", "missing", "method", "estimator")

setkeyv(d, grps)

d[, `:=`(
  warn_lgl = warn != "",
  errs_lgl = errs != "",
  valid = warn == "" & errs == ""
)]



summary <- d[rep <= 1000, .(
  valid = sum(warn == "" & errs == "") / 1000,
  num_warn = sum(warn != ""),
  num_errs = sum(errs != "")

  ), by = grps]

summary[, .(flag = sum(valid < .95))]
summary[, .(flag = min(valid))]
summary[, .(sum(flag)), by = grps]
lapply(summary[flag == TRUE, ..grps], table)

hist(summary$valid[summary$flag])


ggplot(data = summary, aes(x = method, y = estimator, alpha = valid)) +
  geom_tile() +
  facet_grid(shape + int + n ~ meas_var + cor + missing)





d2 <- d[!(!valid | is.na(valid)) & !(method == "DMC" & meas_var == .3), `:=`(
  new_rep = seq(.N),
  est_lo = est - qnorm(.975) * se,
  est_hi = est + qnorm(.975) * se,
  rejected = pval <= .05
), by = grps]
d2 <- d2[new_rep <= 1000]
d2 <- d2[, `:=`(
  covered = est_lo < b3 & b3 < est_hi
)]

set.seed(12479) # for bootstrapping
d2[, c("se_std_lo", "se_std_hi") := se_bias_boot(se, est, r = 1000), by = grps]

grps2 <- grps[!grps %in% "missing"]
dre <- dcast(d2, shape + int + cor + meas_var + n  + method + estimator+ rep  ~ missing, value.var = "est")
dre <- na.omit(dre)
dre <- dre[, re_boot(complete, pmd, r = 2000), by = grps2]

d3 <- d2[, .(

  # nobs = n,

  repn = .N,

  bias = mean(est - b3),
  bias_sd = sd(est),
  # est_lo = mean(est) - sd(est) / sqrt(.N),
  # est_hi = mean(est) + sd(est) / sqrt(.N),

  std_bias = mean((est - b3) / b3),
  std_bias_sd   = sd(est / b3),
  # est_std_lo =


  se_std_bias  = mean((se - sd(est)) / sd(est)),
  se_std_lo = unique(se_std_lo),
  se_std_hi = unique(se_std_hi),

  coverage = mean(covered),

  rejection = mean(rejected)

), by = grps]



d3[, `:=`(

  bias_lo = bias - qnorm(.975) * bias_sd / sqrt(repn),
  bias_hi = bias + qnorm(.975) * bias_sd / sqrt(repn),

  std_bias_lo = std_bias - qnorm(.975) * std_bias_sd / sqrt(repn),
  std_bias_hi = std_bias + qnorm(.975) * std_bias_sd / sqrt(repn)

)]

d3[, c("coverage_lo", "coverage_hi") := binom.confint(coverage * repn, repn, methods = "exact")[5:6]]
d3[, c("rejection_lo", "rejection_hi") := binom.confint(rejection * repn, repn, methods = "exact")[5:6]]

d3 <- merge(d3, dre)

fwrite(d3, "final_summaries.csv", sep = ",")
