suppressMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
})

#### aggregate_cdfs() ####
test_that("Test aggregate_cdfs(): no trim vinc",{
  quant <- seq(0,1,0.5)
  d <- expand.grid(model = c("A","B"),
                   quantile = quant)
  d$value <- d$quantile *2
  test <- aggregate_cdfs(d, by = "model", method = vincent, ret_quantiles = quant)
  expected <- tibble(quantile = quant, value = quant * 2)
  expect_equal(test, expected)
})

test_that("Test aggregate_cdfs(): no trim LOP",{
  quant <- seq(0,1,0.5)
  d <- expand.grid(model = c("A","B"),
                   quantile = quant)
  d$value <- d$quantile *2
  test <- aggregate_cdfs(d, by = "model", method = LOP, ret_quantiles = quant)
  expected <- tibble(quantile = quant, value = quant * 2)
  expect_equal(test, expected)
})

test_that("Test aggregate_cdfs(): no trim LOP, different ret_quant",{
  quant <- seq(0,1,0.5)
  ret_quant <- seq(0,1,0.1)
  d <- expand.grid(model = c("A","B"),
                   quantile = quant)
  d$value <- d$quantile *2
  test <- aggregate_cdfs(d, by = "model", method = LOP, ret_quantiles = ret_quant)
  expected <- tibble(quantile = ret_quant, value = ret_quant * 2)
  expect_equal(test, expected)
})

test_that("Test aggregate_cdfs(): mean interior trim vinc",{
  quant <- seq(0,1,0.5)
  d <- expand.grid(model = c("A","B","C"),
                   quantile = quant)
  d$value <- ifelse(d$model == "A", d$quantile * 2, ifelse(d$model == "B", d$quantile * 3, d$quantile * 4))
  test <- aggregate_cdfs(d, by = "model", method = vincent, ret_quantiles = quant, trim = "mean_interior", n_trim = 1)
  expected <- tibble(quantile = quant, value = quant * 3)
  expect_equal(test, expected)
})



#### update_by_col() ####
test_that("Test update_id_var_col()",{
  d <- expand.grid(model = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- 1
  expected <- d
  colnames(expected)[1] = "id"
  expect_equal(update_id_var_col(d, "model"), expected)
})

#### filter_input_data() ####
test_that("Test filter_input_data(): NA vals",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- d$quantile *2
  d[1,3] = NA
  expect_warning(filter_input_data(d))
  test = suppressWarnings(filter_input_data(d))
  expect_equal(test, d %>% filter(id != "A"))
})

test_that("Test filter_input_data(): non-monotonic",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- d$quantile *2
  d[1,3] = NA
  expect_warning(filter_input_data(d))
  test = suppressWarnings(filter_input_data(d))
  expect_equal(test, d %>% filter(id != "A"))
})

test_that("Test filter_input_data(): one val",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- ifelse(d$id == "A", 1, d$quantile*2)
  expect_warning(filter_input_data(d))
  test = suppressWarnings(filter_input_data(d))
  expect_equal(test, d %>% filter(id != "A"))
})

test_that("Test filter_input_data(): multiple",{
  d <- expand.grid(id = c("A","B","C"),
                   quantile = seq(0,1,0.5))
  d$value <- ifelse(d$id == "A", 1, d$quantile*2)
  d[nrow(d),3] = NA
  expect_warning(filter_input_data(d))
  test = suppressWarnings(filter_input_data(d))
  expect_equal(test, d %>% filter(id == "B"))
})


#### check_na_vals() ####
test_that("Test check_na_vals()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- 1
  d[1,3] = NA
  expect_warning(check_na_vals(d))
  test = suppressWarnings(check_na_vals(d))
  expect_equal(test, "A")
  d[2,3] = NA
  test = suppressWarnings(check_na_vals(d))
  expect_equal(test, c("A","B"))
})

#### check_monotonic() ####
test_that("Test check_monotonic()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- d$quantile * 2
  d[nrow(d),3] = 0
  expect_warning(check_monotonic(d))
  test = suppressWarnings(check_monotonic(d))
  expect_equal(test, "B")
  d[nrow(d) - 1,3] = 0
  test = suppressWarnings(check_monotonic(d))
  expect_equal(test, c("A","B"))
})

#### check_num_unq_vals() ###
test_that("Test check_num_unq_vals()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- ifelse(d$id == "A", 1, d$quantile*2)
  expect_warning(check_num_unq_vals(d))
  test = suppressWarnings(check_num_unq_vals(d))
  expect_equal(test, "A")
  d$value = 1
  test = suppressWarnings(check_num_unq_vals(d))
  expect_equal(test, c("A","B"))
})

