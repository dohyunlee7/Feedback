# I don't understand why it isn't picking up the
# crappy indentation?
test_that("first style test", {
  expect_equal(styleCheck_file("foo.R"), 12.857143)
})
