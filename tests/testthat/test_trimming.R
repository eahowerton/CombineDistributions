library("testthat")
library("dplyr")
# source("src/aggregation_methods/trimming.R")
source_test_helpers("src/aggregation_methods/LOP.R")
source_test_helpers("src/aggregation_methods/vincent.R")


#### mean_trim_ids() ####
test_that("Test mean_trim_ids()",{
  d <- expand.grid(mean = 1:4,
                    quantile = seq(0.01,0.99,0.01))
  d$id <- LETTERS[d$mean]
  d <- d %>% mutate(value = qnorm(d$quantile, d$mean, 1)) %>% select(id, quantile, value)
  kv <- list(1:3, c(2,4),1) # can add more combinations to this list, check one of each length for now
  for (i in 1:length(kv)){
    keep_vals <- kv[[i]]
    expect_equal(mean_trim_ids(d, keep_vals), LETTERS[keep_vals])
  }
})


#### mean_trim() ####
test_that("Test mean_trim(): trim all but one, exterior",{
  eps <- 0.001
  quant <- seq(0,1,0.01)
  n_trim <- 2
  trim_type <- "exterior"
  d <- expand.grid(min = 1:3,
                   quantile = quant)
  d$id <- LETTERS[d$min]
  d <- d %>% mutate(value = qunif(d$quantile, d$min, d$min+1)) %>% select(id, quantile, value)
  # test output LOP
  avg_dir <- "LOP"
  test <- mean_trim(d, n_trim, trim_type, avg_dir, quant)
  expect_equal(test$value, d[d$id == "B","value"])
  # test output vincent
  avg_dir <- "vincent"
  test <- mean_trim(d, n_trim, trim_type, avg_dir, quant)
  expect_equal(test$value, d[d$id == "B","value"])
})

test_that("Test mean_trim(): trim all but one, interior",{
  eps <- 0.001
  quant <- seq(0,1,0.01)
  n_trim <- 1
  trim_type <- "interior"
  d <- expand.grid(id = LETTERS[1:3],
                   quantile = quant)
  d <- d %>% mutate(value = qunif(d$quantile))
  # test output LOP
  avg_dir <- "LOP"
  test <- mean_trim(d, n_trim, trim_type, avg_dir, quant)
  expect_equal(test$value, d[d$id == "B","value"])
  # test output vincent
  avg_dir <- "vincent"
  test <- mean_trim(d, n_trim, trim_type, avg_dir, quant)
  expect_equal(test$value, d[d$id == "B","value"])
})


#### cdf_trim() ####
test_that("Test cdf_trim()",{
  eps <- 0.001
  quant <- seq(0,1,0.01)
  n_trim <- 2
  trim_type <- "exterior"
  d <- expand.grid(min = 1:3,
                   quantile = quant)
  d$id = LETTERS[d$min]
  d$max = ifelse(d$id == "B", 3, 4)
  d <- d %>% mutate(value = qunif(d$quantile, d$min, d$max)) %>% select(id, quantile, value)
  # test output LOP
  avg_dir <- "LOP"
  test <- cdf_trim(d, n_trim, trim_type, avg_dir, quant)
  expect_true(all(test$value - c(seq(2,2.5, 0.01), seq(2.53, 4, 0.03))<eps))
  # test output vincent
  avg_dir <- "vincent"
  test <- cdf_trim(d, n_trim, trim_type, avg_dir, quant)
  expect_true(all(test$value - c(seq(2,2.5, 0.01), seq(2.53, 4, 0.03))<eps))
})

#### cdf_mean() ####
test_that("Test cdf_mean: parametric distribution",{
  eps <- 0.001
  d <- data.frame(quantile = seq(0.01,0.99,0.01),
                  value = qnorm(seq(0.01,0.99,0.01),0,1))
  expected <- 0
  actual <- cdf_mean(d)
  expect_lt(abs(expected - actual), eps)
})

test_that("Test cdf_mean: ecdf",{
  eps <- 0.001
  d <- data.frame(quantile = seq(0.01,0.99,0.01),
                  value = quantile(rnorm(1000),seq(0.01,0.99,0.01)))
  expected <- 0
  actual <- cdf_mean(d)
  expect_lt(abs(expected - actual), eps)
})


