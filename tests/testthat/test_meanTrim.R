suppressWarnings({
  library("testthat")
  library("dplyr")
})

source_test_helpers("R/determineTrimVals.R")


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
  test <- mean_trim(d, trim_type, n_trim)
  expect_equal(test, d %>% mutate(weight = ifelse(id == "B", 1, 0)))
})

test_that("Test mean_trim(): trim all but one, exterior",{
  eps <- 0.001
  quant <- seq(0,1,0.01)
  n_trim <- 1
  trim_type <- "interior"
  d <- expand.grid(min = 1:3,
                   quantile = quant)
  d$id <- LETTERS[d$min]
  d <- d %>% mutate(value = qunif(d$quantile, d$min, d$min+1)) %>% select(id, quantile, value)
  test <- mean_trim(d, trim_type, n_trim)
  expect_equal(test, d %>% mutate(weight = ifelse(id == "B", 0, 1)))
})


#### approx_cdf_mean() ####
test_that("Test approx_cdf_mean: parametric distribution",{
  eps <- 0.001
  d <- data.frame(quantile = seq(0.01,0.99,0.01),
                  value = qnorm(seq(0.01,0.99,0.01),0,1))
  expected <- 0
  actual <- approx_cdf_mean(d)
  expect_lt(abs(expected - actual), eps)
})

test_that("Test approx_cdf_mean: ecdf",{
  eps <- 0.001
  d <- data.frame(quantile = seq(0.01,0.99,0.01),
                  value = quantile(rnorm(1000),seq(0.01,0.99,0.01)))
  expected <- 0
  actual <- approx_cdf_mean(d)
  expect_lt(abs(expected - actual), eps)
})

#### find_mean_trim_ids() ####
test_that("Test find_mean_trim_ids()",{
  d <- expand.grid(mean = 1:4,
                   quantile = seq(0.01,0.99,0.01))
  d$id <- LETTERS[d$mean]
  d <- d %>% mutate(value = qnorm(d$quantile, d$mean, 1)) %>% select(id, quantile, value)
  kv <- list(1:3, c(2,4),1) # can add more combinations to this list, check one of each length for now
  for (i in 1:length(kv)){
    keep_vals <- kv[[i]]
    expect_equal(find_mean_trim_ids(d, keep_vals), LETTERS[keep_vals])
  }
})


#### implement_mean_trim() ####
test_that("Test implement_mean_trim()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.1))
  d <- d %>% mutate(value = rnorm(22))
  keep_ids <- c("A")
  expected <- d %>% mutate(weight = ifelse(id == "A", 1, 0))
  expect_equal(implement_mean_trim(d, keep_ids), expected)
})


