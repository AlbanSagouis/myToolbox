context("Testing increment_version_numbers")

test_that("Wrong parameter", {
   expect_error(incVer(pkg='myToolbox', folder='.', increase = 'test'))
})
