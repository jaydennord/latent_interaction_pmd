checkpoint::checkpoint("2019-10-01", scanForPackages = FALSE)

library(data.table)
library(MplusAutomation)
library(tidyverse)
library(parallel)


dsn2 <- fread("dsn2.csv", sep = ",")

extract_results <- function(file, ...) {

  model <- readModels(file, what = "parameters", quiet = TRUE)

  params <- tryCatch(
    subset(
      model$parameters$unstandardized,
      paramHeader == "Y.ON" & param == "XZ",
      c(est, se, pval)
    ),
    error = function(e) data.frame(
      est =  NA_real_,
      se  =  NA_real_,
      pval = NA_real_)
  )

  cbind(params,tibble(
    warning = list(model$warnings),
    error   = list(model$errors)
  ))

}

results <- mclapply(dsn2$out, extract_results, mc.cores = detectCores()) %>% bind_rows()

final <- bind_cols(dsn2, results)

fwrite(final, "final_results.csv")
