library("testthat")

source_test_helpers("R/utils.R")
source_test_helpers("R/determineTrimVals.R")
source_test_helpers("R/cdfTrim.R")
source_test_helpers("R/meanTrim.R")

#### calculate_aggregate_vin() ####
test_that("Test vincent(): simple example",{
  d <- data.frame(quantile = c(0,0,0.5,0.5,1,1),
                  value = c(1,2,3,4,5,6),
                  weight = rep(1,6))
  d$id <- "A"
  expected <- tibble(quantile = c(0,0.5,1),
                         value = c(1.5,3.5,5.5))
  expect_identical(calculate_aggregate_vin(d, c(0,0.5,1)), expected)
})

test_that("Test vincent: single CDF uniform",{
  quant <- (0:100)/100
  d <- expand.grid(id = c("A"),
                   quantile = quant)
  d$value <-  3*d$quantile
  expected <- 3*quant
  expect_equal(vincent(d$quantile, d$value, d$id, quant), expected)
})

test_that("Test vincent: multiple CDF uniform",{
  quant <- (0:100)/100
  d <- expand.grid(id = c("A","B"),
                   quantile = quant)
  d$min <- ifelse(d$id == "A", 0, 1)
  d <- d %>% mutate(value = qunif(d$quantile, d$min, d$min + 2)) %>% select(-min)
  expected <- seq(0.5,2.5,0.02)
  expect_equal(vincent(d$quantile, d$value, d$id, quant), expected)
})

test_that("Test vincent: single CDF normal",{
  quant = (0:100)/100
  d <- expand.grid(id = c("A"),
                   quantile = quant)
  d$value <-  qnorm(d$quantile)
  expected <- qnorm(quant)
  expect_identical(vincent(d$quantile, d$value, d$id, quant), expected)
})

test_that("Test vincent: multiple CDF normal",{
  quant <- (0:100)/100
  d <- expand.grid(mean = 1:2,
                   quantile = quant)
  d <- d %>%
    mutate(id = LETTERS[mean],
           value = qnorm(quantile, mean, mean)) %>%
    select(id, quantile, value)
  expected <- qnorm(quant, 1.5, 1.5)
  expect_equal(vincent(d$quantile, d$value, d$id, quant), expected)
})

test_that("Test vincent: return subset of quantiles",{
  quant <- (0:100)/100
  ret_quant <- seq(0,1,0.05)
  d <- expand.grid(mean = 1:2,
                   quantile = quant)
  d <- d %>%
    mutate(id = LETTERS[mean],
           value = qnorm(quantile, mean, mean)) %>%
    select(id, quantile, value)
  expected <- qnorm(ret_quant, 1.5, 1.5)
  expect_equal(vincent(d$quantile, d$value, d$id, ret_quant), expected)
})

test_that("Test vincent: return additional quantiles",{
  quant <- seq(0,1,0.05)
  ret_quant <- (5:95)/100
  d <- expand.grid(mean = 1:2,
                   quantile = quant)
  d <- d %>%
    mutate(id = LETTERS[mean],
           value = qnorm(quantile, mean, mean)) %>%
    select(id, quantile, value)
  expected <- approx(quant, qnorm(quant, 1.5, 1.5), ret_quant)$y
  expect_equal(vincent(d$quantile, d$value, d$id, ret_quant), expected)
})

#### evaluate_qf() ####
test_that("Test evaluate_qf(): interpolation of quantiles",{
  d <- data.frame(id = c(rep("A", 3), rep("B",3)),
                  values = c(0, 3, 10, 0,5,10),
                  quantiles = c(0,0.3,1,0,0.5,1))
  # check a set of values
  quants <- 1:100/100
  expected <- data.frame(quantile = rep(quants, 2),
                         id = sort(rep(c("A","B"), length(quants))),
                         value = rep(quants*10,2))
  expect_equal(evaluate_qf(d$quantile, d$value, d$id, quants), expected)
})

