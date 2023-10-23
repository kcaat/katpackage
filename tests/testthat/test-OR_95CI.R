test_that("OR_95CI function works", {
  expect_equal(OR_95CI(1.3,0.8,0.05,2), "3.67 (0.76, 17.60)")
})
