#source("src/aggregation_methods/LOP.R")
suppressMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
})

source_test_helpers("R/utils.R")
source_test_helpers("R/determineTrimVals.R")
source_test_helpers("R/cdfTrim.R")
source_test_helpers("R/meanTrim.R")



#### LOP ####
test_that("Test LOP: single CDF uniform",{
  quant <- (0:100)/100
  d <- expand.grid(id = c("A"),
                   quantile = quant)
  d$value <-  3*d$quantile
  expect_equal(LOP(d$quantile, d$value, d$id, quant), 3*quant)
})

test_that("Test calculate_aggregate_LOP: multiple CDF uniform",{
  quant <- (0:100)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$min <- ifelse(d$id == "A", 0, 1)
  d <- d %>% mutate(value = qunif(d$quantile, d$min, d$min + 2)) %>% select(-min)
  test <- LOP(d$quantile, d$value, d$id, quant)
  expected <- c(seq(0,1,0.04),seq(1.02,2,0.02), seq(2.04,3,0.04))
  expect_equal(test, expected)
})

test_that("Test LOP: single CDF normal",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A"),
                   quantile = quant)
  d$value <-  qnorm(d$quantile)
  expected <- qnorm(quant)
  test <- LOP(d$quantile, d$value, d$id, quant)
  # check output content
  expect_true(all(abs(test[c(-1,-length(test))] - expected[c(-1,-length(test))]) < eps))
})

test_that("Test LOP: same distribution",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$value <- qnorm(d$quantile)
  expected <- qnorm(quant)
  test <- LOP(d$quantile, d$value, d$id, quant)
  expect_true(all(abs(test[c(-1,- length(test))] - expected[c(-1,- length(test))]) < eps))
})

test_that("Test LOP: cdf_trim",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B", "C"),
                   quantile = quant)
  d$mean <- ifelse(d$id == "A", -1, ifelse(d$id == "B",0,1))
  d <- d %>% mutate(value = qnorm(d$quantile,d$mean, 1)) %>% select(-mean)
  expected <- qnorm(quant)
  test <- LOP(d$quantile, d$value, d$id, quant,
              weight_fn = cdf_trim, n_trim = 2, trim_type = "exterior", avg_dir = "LOP")
  expect_true(all(abs(test[c(-1,- length(test))] - expected[c(-1,- length(test))]) < eps))
})

test_that("Test LOP: mean_trim",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B", "C"),
                   quantile = quant)
  d$mean <- ifelse(d$id == "A", -1, ifelse(d$id == "B",0,1))
  d <- d %>% mutate(value = qnorm(d$quantile,d$mean, 1)) %>% select(-mean)
  expected <- qnorm(quant)
  test <- LOP(d$quantile, d$value, d$id, quant,
              weight_fn = mean_trim, n_trim = 2, trim_type = "exterior")
  expect_true(all(abs(test[c(-1,- length(test))] - expected[c(-1,- length(test))]) < eps))
})

test_that("Test LOP: same mean",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B", "C"),
                   quantile = quant)
  d$sd <- ifelse(d$id == "A", 0.5, ifelse(d$id == "B",1,2))
  d <- d %>% mutate(value = qnorm(d$quantile,0, d$sd)) %>% select(-sd)
  test <- LOP(d$quantile, d$value, d$id, quant)
  test <- test[!is.na(test)]
  test_mean <- sapply(1:(length(test)-1),
                      function(i){((test[i]+test[i+1])/2)*((quant[i+1]-quant[i]))})
  test_mean <- sum(test_mean)
  # expected mean == 0
  expect_lt(abs(test_mean),eps)
})

test_that("Test LOP: return different quantiles",{
  eps <- 0.001
  quant <- (1:99)/100
  ret_quant <- c(0.25,0.5,0.75)
  d <- expand.grid(id = c("A"),
                   quantile = quant)
  d$value <-  qnorm(d$quantile)
  expected <- qnorm(ret_quant)
  test <- LOP(d$quantile, d$value, d$id, ret_quant)
  expect_true(all(abs(test[c(-1,- length(test))] - expected[c(-1,- length(test))]) < eps))
})

# test_that("Test calculate_aggregate_LOP: real world SEJ?",{
#
# })


#### evaluate_cdf() ####
test_that("Test evaluate_cdf(): calculation of values",{
  quant <- seq(0,1,0.1)
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$value <- ifelse(d$id == "A", 2*d$quantile, 3*d$quantile)
  # check a set of values
  vals <- 0:3
  expected <- data.frame(quantile = c(0,0.5,1,1,0,1/3,2/3,1),
                         id = sort(rep(c("A","B"), length(vals))),
                         value = rep(vals,2))
  expect_equal(evaluate_cdf(d$quantile, d$value, d$id, vals), expected) # TO DO: figure out why not identical
})


#### avg_probs() ####
test_that("Test avg_probs()",{
  df_cdfs <- data.frame(quantile = c(0,0.5,0.5,1,1,1),
                        value = c(1,1,2,2,3,3),
                        weight = rep(0.5,6))
  expected <- tibble(value = c(1,2,3),
                     quantile = c(0.25,0.75,1))
  expect_identical(avg_probs(df_cdfs), expected)
})

