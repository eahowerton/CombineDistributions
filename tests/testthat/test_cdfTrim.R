suppressMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
})
source_test_helpers("R/determineTrimVals.R")


#### cdf_trim() ####
test_that("Test cdf_trim()", {
  df_long <- data.frame(id = c(rep("A",3), rep("B",3),rep("C",3)),
                        value = c(1,2,3,1,2,3,1,2,3),
                        quantile = c(0,0.2,0.4,0.1,0.3,0.5,0.2,0.4,0.6))
  trim_type = "interior"
  n_trim = 1
  # check a set of keep values
  expected <- tibble(id = c(rep("A",3), rep("B",3),rep("C",3)),
                     value = c(1,2,3,1,2,3,1,2,3),
                     quantile = c(0,0.2,0.4,0.1,0.3,0.5,0.2,0.4,0.6),
                     weight = c(rep(1,3), rep(0,3),rep(1,3))) %>%
    group_by(value)
  expect_identical(cdf_trim(df_long, trim_type, n_trim), expected)
})

#### trim_cdf() ####
test_that("Test implement_trim_cdf()",{
  df_long <- data.frame(quantile = c(0,0.5,1,0,0,0.5),
                        value = c(1,1,1,2,2,2))
  # check a set of keep values
  keep = 1:2
  expected <- df_long %>%
    mutate(weight = c(1,1,0,1,1,0)) %>%
    group_by(value)
  expect_identical(implement_trim_cdf(df_long, keep), expected)
  # check a second set of keep values
  keep = c(1,3)
  expected <- df_long %>%
    mutate(weight = c(1,0,1,1,0,1)) %>%
    group_by(value)
  expect_identical(implement_trim_cdf(df_long, keep), expected)
})

