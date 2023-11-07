# First testing of correctness
test_that("first correctness test", {
  expect_equal(as.numeric(correctCheck_file("foo.R",
                                            "solution.R")$correctCheck['Total']),
               100)
})
