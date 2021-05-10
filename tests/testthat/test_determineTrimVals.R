library("testthat")

#### check keep_vals() ####
test_that("Test keep_vals()",{
  # exterior
  expect_setequal(keep_vals("exterior", 2, 6)[["keep"]], 2:5)
  expect_setequal(suppressWarnings(keep_vals("exterior", 3, 6)[["keep"]]), 3:4)
  expect_warning(keep_vals("exterior", 3, 6))
  # interior
  expect_setequal(keep_vals("interior", 2, 6)[["keep"]], c(1:2, 5:6))
  expect_setequal(suppressWarnings(keep_vals("interior", 3, 6)[["keep"]]), c(1,6))
  expect_warning(keep_vals("interior", 3, 6))
  # fail
  expect_error(suppressWarnings(keep_vals("interior", 5, 6)))
  expect_error(keep_vals("interior", 0, 6))
})

#### check_update_ntrim_even() ####
test_that("Test check_update_ntrim_even()",{
  expect_equal(check_update_ntrim_even(2), 2)
  expect_equal(suppressWarnings(check_update_ntrim_even(3)), 4)
  expect_warning(check_update_ntrim_even(3))
})

#### check_update_ntrim_sameparity() ####
test_that("Test check_update_ntrim_sameparity()",{
  # pass
  expect_equal(check_update_ntrim_sameparity(2,4), 2)
  expect_equal(check_update_ntrim_sameparity(3,5), 3)
  # warning
  expect_equal(suppressWarnings(check_update_ntrim_sameparity(2,5)), 3)
  expect_warning(check_update_ntrim_sameparity(2,5))
  expect_equal(suppressWarnings(check_update_ntrim_sameparity(3,6)), 4)
  expect_warning(check_update_ntrim_sameparity(3,6))
})

#### check_ntrim_vs_nid() ####
test_that("Test check_ntrim_vs_nid()",{
  expect_error(check_ntrim_vs_nid(0,4))
  expect_error(check_ntrim_vs_nid(-1,4))
  expect_error(check_ntrim_vs_nid(5,4))
})

#### keep_vals_exterior() ####
test_that("Test keep_vals_exterior(): odd number of models",{
  ## odd number of models
  n_models <- 5
  # check keep
  expect_setequal(keep_vals_exterior(2, n_models)[["keep"]] , 2:4)
  expect_setequal(keep_vals_exterior(4, n_models)[["keep"]] , 3)
  # check n_trim
  expect_equal(keep_vals_exterior(2, n_models)[["n_trim"]] , 2)
  expect_equal(keep_vals_exterior(4, n_models)[["n_trim"]] , 4)
})

test_that("Test keep_vals_exterior(): even number of models",{
  ## even number of models
  n_models <- 6
  # check keep
  expect_setequal(keep_vals_exterior(2, n_models)[["keep"]] , 2:5)
  expect_setequal(keep_vals_exterior(4, n_models)[["keep"]] , 3:4)
  # check n_trim
  expect_equal(keep_vals_exterior(2, n_models)[["n_trim"]] , 2)
  expect_equal(keep_vals_exterior(4, n_models)[["n_trim"]] , 4)
})

#### keep_vals_interior() ####
test_that("Test keep_vals_interior: odd number of models",{
  n_models <- 5
  # check keep
  expect_setequal(keep_vals_interior(1, n_models)[["keep"]] , c(1,2,4,5))
  expect_setequal(keep_vals_interior(3, n_models)[["keep"]] , c(1,5))
  # check n_trim
  expect_equal(keep_vals_interior(1, n_models)[["n_trim"]] , 1)
  expect_equal(keep_vals_interior(3, n_models)[["n_trim"]] , 3)
})

test_that("Test keep_vals_interior: even number of models",{
  n_models <- 6
  # check keep
  expect_setequal(keep_vals_interior(2, n_models)[["keep"]] , c(1,2,5,6))
  expect_setequal(keep_vals_interior(4, n_models)[["keep"]] , c(1,6))
  # check n_trim
  expect_equal(keep_vals_interior(2, n_models)[["n_trim"]] , 2)
  expect_equal(keep_vals_interior(4, n_models)[["n_trim"]] , 4)
})

