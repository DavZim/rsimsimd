test_that("Test that dist_cosine fails expectedly", {
  expect_error(dist_cosine("a"), regexp = "x must be numeric")
  expect_error(dist_cosine(c(1, 2, 3), "a"), regexp = "y must be numeric")
  expect_error(dist_cosine(c(1, 2, 3), c(1, 2)),
               regexp = "x and y must have the same length")
  expect_error(dist_cosine(matrix(seq(9) + 0.01, nrow = 3),
                           matrix(seq(4) + 0.01, nrow = 2)),
               regexp = "Each element of x and y must have the same length")
})


test_that("Test that dist_cosine returns the correct dimensions", {
  n <- 10 # number of embeddings

  # check dimensions
  # 1-1
  expect_equal(length(dist_cosine(rnorm(n), rnorm(n))), 1)
  # 1-many
  expect_equal(length(dist_cosine(matrix(rnorm(n * 10), ncol = n), rnorm(n))),
               10)

  # many-1
  expect_equal(length(dist_cosine(rnorm(n), matrix(rnorm(n * 10), ncol = n))),
               10)

  # 3 elements x 5 elements results in 3 x 5 matrix
  mat_3 <- matrix(rnorm(3 * n), ncol = n)
  mat_5 <- matrix(rnorm(5 * n), ncol = n)
  expect_equal(dim(dist_cosine(mat_3, mat_5)),
               c(3, 5))

  # single mat => n x n matrix
  expect_equal(dim(dist_cosine(mat_3)), c(3, 3))
})
