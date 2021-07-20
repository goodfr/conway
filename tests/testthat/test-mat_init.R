test_that("Initialisation", {

	.dfr <- conway::mat_init(.seed = 789, n = 40, square_sequence = -5:5, ndim = 2)

	testthat::expect_equal(object = nrow(.dfr), expected = 40)

})
