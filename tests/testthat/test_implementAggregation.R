suppressMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
  library(data.table)
})

source_test_helpers("R/utils.R")
source_test_helpers("R/vincent.R")
source_test_helpers("R/determineTrimVals.R")
source_test_helpers("R/cdfTrim.R")
source_test_helpers("R/meanTrim.R")

#### calculate_single_aggregate() ####
test_that("Test calculate_single_aggregate(): no trim vinc",{
  quant <- seq(0,1,0.5)
  d <- expand.grid(model = c("A","B"),
                   quantile = quant)
  d$value <- d$quantile *2
  test <- calculate_single_aggregate(d$quantile, d$value, d$model,
                                     method = "vincent", ret_quantiles = quant)
  expected <- data.frame(quantile = quant, value = quant * 2)
  expect_equal(test, expected)
})

test_that("Test calculate_single_aggregate(): no trim LOP",{
  quant <- seq(0,1,0.5)
  d <- expand.grid(model = c("A","B"),
                   quantile = quant)
  d$value <- d$quantile *2
  test <- calculate_single_aggregate(d$quantile, d$value, d$model,
                                     method = "LOP", ret_quantiles = quant)
  expected <- data.frame(quantile = quant, value = quant * 2)
  expect_equal(test, expected)
})

test_that("Test calculate_single_aggregate(): no trim LOP, different ret_quant",{
  quant <- seq(0,1,0.5)
  ret_quant <- seq(0,1,0.1)
  d <- expand.grid(model = c("A","B"),
                   quantile = quant)
  d$value <- d$quantile *2
  test <- calculate_single_aggregate(d$quantile, d$value, d$model,
                                     method = "LOP", ret_quantiles = ret_quant)
  expected <- data.frame(quantile = ret_quant, value = ret_quant * 2)
  expect_equal(test, expected)
})

test_that("Test calculate_single_aggregate(): mean interior trim vinc",{
  quant <- seq(0,1,0.5)
  d <- expand.grid(model = c("A","B","C"),
                   quantile = quant)
  d$value <- ifelse(d$model == "A", d$quantile * 2, ifelse(d$model == "B", d$quantile * 3, d$quantile * 4))
  test <- calculate_single_aggregate(d$quantile, d$value, d$model,
                                     method = "vincent", ret_quantiles = quant, trim = "mean_interior", n_trim = 1)
  expected <- data.frame(quantile = quant, value = quant * 3)
  expect_equal(test, expected)
})

test_that("Test calculate_single_aggregate(): odd quantiles",{
  quant <- seq(0,1,0.5)
  d <- expand.grid(model = c("A","B"),
                   quantile = quant)
  d$value <- d$quantile *2
  test <- calculate_single_aggregate(d$quantile, d$value, d$model,
                                     method = "vincent", ret_quantiles = c(0.1,0.33,0.7))
  expected <- data.frame(quantile = c(0.1,0.33,0.7), value = c(0.1,0.33,0.7) * 2)
  expect_equal(test, expected)
})

#### update_id_var_col() ####
test_that("Test update_id_var_col()",{
  d <- expand.grid(model = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- 1
  expected <- d
  colnames(expected)[1] = "id"
  expect_equal(update_id_var_col(d, "model"), expected)
})

#### filter_input_data() ####
test_that("Test prep_input_data(): NA vals",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- d$quantile *2
  d[1,3] = NA
  expect_warning(prep_input_data(d$quantile, d$value, d$id))
  test = suppressWarnings(prep_input_data(d$quantile, d$value, d$id))
  expect_equal(as.matrix(test), as.matrix(d %>% filter(id != "A")))
})

test_that("Test filter_input_data(): non-monotonic",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- d$quantile *2
  d[1,3] = NA
  expect_warning(prep_input_data(d$quantile, d$value, d$id))
  test = suppressWarnings(prep_input_data(d$quantile, d$value, d$id))
  expect_equal(as.matrix(test), as.matrix(d %>% filter(id != "A")))
})

test_that("Test filter_input_data(): multiple",{
  d <- expand.grid(id = c("A","B","C"),
                   quantile = seq(0,1,0.5))
  d$value <- ifelse(d$id == "A", d$quantile, d$quantile*2)
  d$value[d$id == "A" & d$quantile == 0.5] = 2
  d[nrow(d),3] = NA
  expect_warning(prep_input_data(d$quantile, d$value, d$id))
  test = suppressWarnings(prep_input_data(d$quantile, d$value, d$id))
  expect_equal(as.matrix(test), as.matrix(d %>% filter(id == "B")))
})


#### check_na_vals() ####
test_that("Test check_na_vals()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- 1
  d[1,3] = NA
  d <- setDT(d)
  expect_warning(check_na_vals(d))
  test = suppressWarnings(check_na_vals(d))
  expect_equal(as.character(test$id), "A")
  d[2,3] = NA
  test = suppressWarnings(check_na_vals(d))
  expect_equal(as.character(test$id), c("A","B"))
})

#### check_monotonic() ####
test_that("Test check_monotonic()",{
  d <- expand.grid(id = c("A","B"),
                   quantile = seq(0,1,0.5))
  d$value <- d$quantile * 2
  d[nrow(d),3] = 0
  d <- setDT(d)
  expect_warning(check_monotonic(d))
  test = suppressWarnings(check_monotonic(d))
  expect_equal(as.character(test$id), "B")
  d[nrow(d) - 1,3] = 0
  test = suppressWarnings(check_monotonic(d))
  expect_equal(as.character(test$id), c("A","B"))
})

