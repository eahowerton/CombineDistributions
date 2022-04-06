suppressWarnings({
  library("testthat")
  library("dplyr")
  library("tibble")
})

test_that("Test remove_zero_weights()",{
  d <- data.frame(quantile = seq(0,1,0.1),
                   value = 1:11,
                   weight = c(0,rep(1,10)))
  expected <- data.frame(quantile = seq(0.1,1,0.1),
                         value = 2:11,
                         weight = rep(1,10))
  expect_equal(remove_zero_weights(d), expected)
})


test_that("Test equal_weights()",{
  d <- expand.grid(id = c("A","B"),
                  quantile = seq(0,1,0.1))
  d$value <- rnorm(nrow(d))
  expected <- d %>% mutate(weight = 1/2)
  expect_equal(equal_weights(d), expected)
})

test_that("Test user_specified_weights()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.1))
  d$value <- rnorm(nrow(d))
  weights <- data.frame(id = c("A", "B"), weight = c(0,1))
  expected <- d %>% mutate(id = as.character(id),
                           weight = ifelse(id == "A", 0, 1))
  expect_equal(user_specified_weights(d, weights), expected)
})

test_that("Test user_specified_weights()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.1))
  d$value <- rnorm(nrow(d))
  weights <- data.frame(m = c("A", "B"), weight = c(0,1))
  expected <- d %>% mutate(id = as.character(id),
                           weight = ifelse(id == "A", 0, 1))
  expect_equal(user_specified_weights(d, weights), expected)
})

test_that("Test user_specified_weights()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.1))
  d$value <- rnorm(nrow(d))
  weights <- data.frame(m = c("A", "B"), weight = c(0,1), extra = 1)
  expect_error(user_specified_weights(d, weights))
})

test_that("Test return_specified_quantiles",{
  d <- data.frame(value = seq(0,100,1))
  d$quantile <- d$value/100
  ret_quantile <- seq(0,1,0.05)
  expected <- tibble(quantile = ret_quantile,
                         value  = ret_quantile*100)
  # check keep
  expect_equal(return_specified_quantiles(d, ret_quantile), expected)
})

test_that("Test return_specified_quantiles: interpolate",{
  d <- data.frame(value = seq(0,100,10))
  d$quantile <- d$value/100
  ret_quantile <- seq(0,1,0.05)
  expected <- tibble(quantile = ret_quantile,
                     value  = ret_quantile*100)
  # check keep
  expect_equal(return_specified_quantiles(d, ret_quantile), expected)
})
