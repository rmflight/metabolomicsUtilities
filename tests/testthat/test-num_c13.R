context("test-num_c13")

test_that("counting c13 works", {
  data("sub_peak_info")
  expect_equal(sub_peak_info$c13, mu_numberc13(sub_peak_info$complete_IMF))
})
