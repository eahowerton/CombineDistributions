#source("src/aggregation_methods/LOP.R")
suppressMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
})


#### calculate_aggregate_LOP ####
test_that("Test calculate_aggregate_LOP: single CDF uniform",{
  quant <- (0:100)/100
  d <- expand.grid(id = c("A"),
                   quantile = quant)
  d$value <-  3*d$quantile
  keep_vals <- c(1)
  expected <- data.frame(quantile = quant,
                         value = 3*quant)
  expect_equal(calculate_aggregate_LOP(d, keep_vals, quant), expected)
})

test_that("Test calculate_aggregate_LOP: multiple CDF uniform",{
  eps <- 0.001
  quant <- (0:100)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$min <- ifelse(d$id == "A", 0, 1)
  d <- d %>% mutate(value = qunif(d$quantile, d$min, d$min + 2)) %>% select(-min)
  keep_vals <- c(1,2)
  test <- calculate_aggregate_LOP(d, keep_vals, quant)
  expected <- data.frame(quantile = quant,
                         value = c(seq(0,1,0.04),seq(1.02,2,0.02), seq(2.04,3,0.04)))
  expect_true(all(abs(test$value[c(-1,-nrow(test))] - expected$value[c(-1,-nrow(test))]) < eps))
})

test_that("Test calculate_aggregate_LOP: single CDF normal",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A"),
                   quantile = quant)
  d$value <-  qnorm(d$quantile)
  keep_vals <- c(1)
  expected <- data.frame(quantile = quant,
                         value = qnorm(quant))
  test <- calculate_aggregate_LOP(d, keep_vals, quant)
  # check characteristics of output
  expect_equal(nrow(test), nrow(expected))
  expect_equal(ncol(test), ncol(expected))
  # check output content
  expect_equal(test$quantile, expected$quantile)
  expect_true(all(abs(test$value[c(-1,-nrow(test))] - expected$value[c(-1,-nrow(test))]) < eps))
})

test_that("Test calculate_aggregate_LOP: same distribution",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$value <- qnorm(d$quantile)
  keep_vals <- c(1,2)
  expected <- data.frame(quantile = quant,
                         value = qnorm(quant))
  test <- calculate_aggregate_LOP(d, keep_vals, quant)
  # check characteristics of output
  expect_equal(nrow(test), nrow(expected))
  expect_equal(ncol(test), ncol(expected))
  # check output content
  expect_equal(test$quantile, expected$quantile)
  expect_true(all(abs(test$value[c(-1,-nrow(test))] - expected$value[c(-1,-nrow(test))]) < eps))
})

test_that("Test calculate_aggregate_LOP: trim",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B", "C"),
                   quantile = quant)
  d$mean <- ifelse(d$id == "A", -1, ifelse(d$id == "B",0,1))
  d <- d %>% mutate(value = qnorm(d$quantile,d$mean, 1)) %>% select(-mean)
  keep_vals <- c(2)
  expected <- data.frame(quantile = quant,
                         value = qnorm(quant))
  test <- calculate_aggregate_LOP(d, keep_vals, quant)
  # check characteristics of output
  expect_equal(nrow(test), nrow(expected))
  expect_equal(ncol(test), ncol(expected))
  # check output content
  expect_equal(test$quantile, expected$quantile)
  expect_true(all(abs(test$value[c(-1,-nrow(test))] - expected$value[c(-1,-nrow(test))]) < eps))
})

test_that("Test calculate_aggregate_LOP: same mean",{
  eps <- 0.001
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B", "C"),
                   quantile = quant)
  d$sd <- ifelse(d$id == "A", 0.5, ifelse(d$id == "B",1,2))
  d <- d %>% mutate(value = qnorm(d$quantile,0, d$sd)) %>% select(-sd)
  keep_vals <- c(1,2,3)
  test <- calculate_aggregate_LOP(d, keep_vals, quant)
  test <- test %>% filter(!is.na(test$value))
  test_mean <- sapply(1:(length(test$value)-1),
                      function(i){((test$value[i]+test$value[i+1])/2)*((test$quantile[i+1]-test$quantile[i]))})
  test_mean <- sum(test_mean)
  # expected mean == 0
  expect_lt(abs(test_mean),eps)
})

test_that("Test calculate_aggregate_LOP: return different quantiles",{
  eps <- 0.001
  quant <- (1:99)/100
  ret_quant <- c(0.25,0.5,0.75)
  d <- expand.grid(id = c("A"),
                   quantile = quant)
  d$value <-  qnorm(d$quantile)
  keep_vals <- c(1)
  expected <- data.frame(quantile = ret_quant,
                         value = qnorm(ret_quant))
  test <- calculate_aggregate_LOP(d, keep_vals, ret_quant)
  # check characteristics of output
  expect_equal(nrow(test), nrow(expected))
  expect_equal(ncol(test), ncol(expected))
  # check output content
  expect_equal(test$quantile, expected$quantile)
  expect_true(all(abs(test$value[c(-1,-nrow(test))] - expected$value[c(-1,-nrow(test))]) < eps))
})

# test_that("Test calculate_aggregate_LOP: real world SEJ?",{
#
# })


#### create_interp_fns() ####
test_that("Test create_interp_fns(): no special cases",{
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$value <- ifelse(d$id == "A", 2*d$quantile, 3*d$quantile)
  test <- create_interp_fns(d)
  # check characteristics of output
  expect_length(test, 2)
  expect_true(is.function(test$A))
  expect_true(is.function(test$B))
  # check content of list
  expect_setequal(test$A(2*quant), quant)
  expect_setequal(test$B(3*quant), quant)
})

test_that("Test create_interp_fns(): special case, all quantiles equal",{
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$value <- ifelse(d$id == "A", 2*d$quantile, 5)
  # check for warning
  expect_warning(create_interp_fns(d))
  suppressWarnings(test <- create_interp_fns(d))
  # check characteristics of output
  expect_length(test, 1)
  expect_true(is.function(test$A))
  # check content of list
  expect_setequal(test$A(2*quant), quant)
})

test_that("Test create_interp_fns(): special case, non-monotonic cdf",{
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$value <- ifelse(d$id == "A", 2*d$quantile, 0)
  d$value[d$id == "B" & d$quantile == 0.5] = 5
  # check for warning
  expect_warning(create_interp_fns(d))
  suppressWarnings(test <- create_interp_fns(d))
  # check characteristics of output
  expect_length(test, 1)
  expect_true(is.function(test$A))
  # check content of list
  expect_setequal(test$A(2*quant), quant)
})

test_that("Test create_interp_fns(): special case, NA quantile",{
  quant <- (1:99)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$value <- ifelse(d$id == "A", 2*d$quantile, 0)
  d$value[d$id == "B" & d$quantile == 0.25] = NA
  # check for warning
  expect_warning(create_interp_fns(d))
  suppressWarnings(test <- create_interp_fns(d))
  # check characteristics of output
  expect_length(test, 1)
  expect_true(is.function(test$A))
  # check content of list
  expect_setequal(test$A(2*quant), quant)
})


#### evaluate_cdf() ####
test_that("Test evaluate_cdf(): calculation of values",{
  interp_fns <- list(approxfun(c(0,2),c(1,1), yleft = 0, yright = 1),
                     approxfun(c(0,2),c(0,0.5), yleft = 0, yright = 1))
  # check a set of values
  vals <- -1:3
  expected <- data.frame(quantile = c(0,rep(1,length(vals)-1), 0,0,0.25,0.5,1),
                         id = sort(rep(c(1,2), length(vals))),
                         value = rep(vals,2))
  expect_equal(evaluate_cdf(vals,interp_fns), expected) # TO DO: figure out why not identical
})


#### trim_cdf() ####
test_that("Test trim_cdf()",{
  df_long <- data.frame(quantile = c(0,0.5,1,0,0,0.5),
                        value = c(1,1,1,2,2,2))
  # check a set of keep values
  keep = 1:2
  expected <- tibble(quantile = c(0,0.5,0,0),
                     value = c(1,1,2,2)) %>%
    group_by(value)
  expect_identical(trim_cdf(df_long, keep), expected)
  # check a second set of keep values
  keep = c(1,3)
  expected <- tibble(quantile = c(0,1,0,0.5),
                     value = c(1,1,2,2)) %>%
    group_by(value)
  expect_identical(trim_cdf(df_long, keep), expected)
})


#### avg_probs() ####
test_that("Test avg_probs()",{
  df_cdfs <- data.frame(quantile = c(0,0.5,0.5,1,0,1),
                        value = c(1,1,2,2,3,3))
  expected <- c(0.25,0.75,0.5)
  # check keep
  expect_identical(avg_probs(df_cdfs), expected)
})


#### LOP_distribution ####
test_that("Test LOP_distribution(): no special cases",{
  vals <- seq(0,100,10)
  gdf_cdf <- vals/100
  expected <- data.frame(quantile = seq(0,1,0.05),
                     value  = seq(0,1,0.05)*100)
  # check keep
  expect_equal(LOP_distribution(gdf_cdf, vals, seq(0,1,0.05)), expected)
})

test_that("Test LOP_distribution(): special cases, extra lower values",{
  vals <- c(0,0,0,seq(0,100,10))
  gdf_cdf <- vals/100
  expected <- data.frame(quantile = seq(0,1,0.05),
                         value  = seq(0,1,0.05)*100)
  # check keep
  expect_equal(LOP_distribution(gdf_cdf, vals, seq(0,1,0.05)), expected)
})

test_that("Test LOP_distribution(): special cases, extra upper values",{
  vals <- c(seq(0,100,10),100,100,100)
  gdf_cdf <- vals/100
  expected <- data.frame(quantile = seq(0,1,0.05),
                         value  = seq(0,1,0.05)*100)
  # check keep
  expect_equal(LOP_distribution(gdf_cdf, vals, seq(0,1,0.05)), expected)
})

test_that("Test LOP_distribution(): special cases, extra lower/upper values",{
  vals <- c(0,0,0,seq(0,100,10),100,100,100)
  gdf_cdf <- vals/100
  expected <- data.frame(quantile = seq(0,1,0.05),
                         value  = seq(0,1,0.05)*100)
  # check keep
  expect_equal(LOP_distribution(gdf_cdf, vals, seq(0,1,0.05)), expected)
})

