test_that("Test that dist_cosine fails expectedly", {
  expect_error(dist_cosine("a"), regexp = "x must be numeric")
  expect_error(dist_cosine(c(1, 2, 3), "a"), regexp = "y must be numeric")
  expect_error(dist_cosine(c(1, 2, 3), c(1, 2)),
               regexp = "x and y must have the same length")
  expect_error(dist_cosine(matrix(seq(9) + 0.01, nrow = 3),
                           matrix(seq(4) + 0.01, nrow = 2)),
               regexp = "x and y must have the same number of columns")
})


test_that("Test that dist_cosine returns the correct dimensions", {
  N <- 10 # number of embeddings

  # check dimensions
  # 1-1
  expect_equal(length(dist_cosine(rnorm(N), rnorm(N))), 1)
  # 1-many
  expect_equal(length(dist_cosine(matrix(rnorm(N * 10), ncol = N), rnorm(N))),
               10)

  # many-1
  expect_equal(length(dist_cosine(rnorm(N), matrix(rnorm(N * 10), ncol = N))),
               10)

  # 3 elements x 5 elements results in 3 x 5 matrix
  mat_3 <- matrix(rnorm(3 * N), ncol = N)
  mat_5 <- matrix(rnorm(5 * N), ncol = N)
  expect_equal(dim(dist_cosine(mat_3, mat_5)),
               c(3, 5))

  # single mat => n x n matrix
  expect_equal(dim(dist_cosine(mat_3)), c(3, 3))
})
