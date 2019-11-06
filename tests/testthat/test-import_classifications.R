context("test-import_classifications")

classification_file = system.file("extdata", "classified_emfs.json", package = "metabolomicsUtilities")

test_that("filtering out classes works", {
  null_filter = import_emf_classifications(classification_file, NULL)
  default_filter = import_emf_classifications(classification_file)
  category_filter = import_emf_classifications(classification_file, list(Sphingolipids = NULL))

  expect_equal(nrow(null_filter) - nrow(default_filter), 44)
  expect_equal(nrow(null_filter) - nrow(category_filter), 1372)
  expect_equal(sum(grepl("^Sphingolipids", category_filter$Categories)), 0)
})
