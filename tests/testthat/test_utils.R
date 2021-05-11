test_that("Test remove_zero_weights()",{
  d <- data.frame(quantile = seq(0,1,0.1),
                   value = 1:11,
                   weight = c(0,rep(1,10)))
  expected <- data.frame(quantile = seq(0.1,1,0.1),
                         value = 2:11,
                         weight = rep(1,10))
  expect_equal(remove_zero_weights(d), expected)
})

