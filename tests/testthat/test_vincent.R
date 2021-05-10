library("testthat")

#### calculate_aggregate_vin() ####
test_that("Test calculate_aggregate_vin(): simple example",{
  d <- data.frame(quantile = c(0,0,0.5,0.5,1,1),
                  value = c(1,2,3,4,5,6))
  expected <- tibble(quantile = c(0,0.5,1),
                         value = c(1.5,3.5,5.5))
  expect_identical(calculate_aggregate_vin(d, c(0,0.5,1)), expected)
})

test_that("Test calculate_aggregate_vin: single CDF uniform",{
  quant <- (0:100)/100
  d <- expand.grid(model = c("A"),
                   quantile = quant)
  d$value <-  3*d$quantile
  expected <- tibble(quantile = quant,
                         value = 3*quant)
  expect_equal(calculate_aggregate_vin(d, quant), expected)
})

test_that("Test calculate_aggregate_vin: multiple CDF uniform",{
  quant <- (0:100)/100
  d <- expand.grid(model = c("A","B"),
                   quantile = quant)
  d$min <- ifelse(d$model == "A", 0, 1)
  d <- d %>% mutate(value = qunif(d$quantile, d$min, d$min + 2)) %>% select(-min)
  expected <- tibble(quantile = quant,
                         value = seq(0.5,2.5,0.02))
  expect_equal(calculate_aggregate_vin(d, quant), expected)
})

test_that("Test calculate_aggregate_vin: single CDF normal",{
  quant = (0:100)/100
  d <- expand.grid(model = c("A"),
                   quantile = quant)
  d$value <-  qnorm(d$quantile)
  expected <- tibble(quantile = quant,
                         value = qnorm(quant))
  expect_identical(calculate_aggregate_vin(d, quant), expected)
})

test_that("Test calculate_aggregate_vin: multiple CDF normal",{
  quant <- (0:100)/100
  d <- expand.grid(mean = 1:2,
                   quantile = quant)
  d <- d %>%
    mutate(model = LETTERS[mean],
           value = qnorm(quantile, mean, mean)) %>%
    select(model, quantile, value)
  expected <- tibble(quantile = quant,
                         value = qnorm(quant, 1.5, 1.5))
  expect_equal(calculate_aggregate_vin(d, quant), expected)
})

test_that("Test calculate_aggregate_vin: return subset of quantiles",{
  quant <- (0:100)/100
  ret_quant <- seq(0,1,0.05)
  d <- expand.grid(mean = 1:2,
                   quantile = quant)
  d <- d %>%
    mutate(model = LETTERS[mean],
           value = qnorm(quantile, mean, mean)) %>%
    select(model, quantile, value)
  expected <- tibble(quantile = ret_quant,
                     value = qnorm(ret_quant, 1.5, 1.5))
  expect_equal(calculate_aggregate_vin(d, ret_quant), expected)
})

test_that("Test calculate_aggregate_vin: return additional quantiles",{
  quant <- seq(0,1,0.05)
  ret_quant <- (5:95)/100
  d <- expand.grid(mean = 1:2,
                   quantile = quant)
  d <- d %>%
    mutate(model = LETTERS[mean],
           value = qnorm(quantile, mean, mean)) %>%
    select(model, quantile, value)
  expected <- tibble(quantile = ret_quant,
                     value = approx(quant, qnorm(quant, 1.5, 1.5), ret_quant)$y)
  expect_warning(calculate_aggregate_vin(d, ret_quant))
  expect_equal(suppressWarnings(calculate_aggregate_vin(d, ret_quant)), expected)
})

