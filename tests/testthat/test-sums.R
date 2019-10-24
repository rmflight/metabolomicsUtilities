context("test-sums")

data("sub_peak_info")
data("normalized_single_emf")
data("single_emf")

test_that("nominal sums work", {
  nom_sum = mu_emf_sum(normalized_single_emf, single_emf)
  expect_known_output(nom_sum, file = "nominal_sums")
})

test_that("weighted sums work", {
  w_sum = mu_emf_sum(normalized_single_emf, single_emf, sub_peak_info)
  expect_known_output(w_sum, file = "weighted_sums")
})