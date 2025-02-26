test_that("Internal Rust functions work", {
  # test against hand-calculated result
  expect_equal(
    rsimsimd:::dist_cosine_rs(c(1, 2, 3), c(4, 5, 6)),
    0.97463173
  )

  # simulate large vectors
  set.seed(123)
  vec1 <- rnorm(1536)
  vec2 <- rnorm(1536)

  # 1-1 relationship
  res <- rsimsimd:::dist_cosine_rs(vec1, vec2)
  expect_equal(res, -0.010641766)


  # one matrix -> all combinations
  list1 <- lapply(seq(3), \(x) rnorm(1536))
  mat1 <- do.call(rbind, list1)
  expect_equal(dim(mat1), c(3, 1536))

  res <- rsimsimd:::dist_cosine_mat_rs(mat1)
  expect_equal(dim(res), c(3, 3))
  # is a symmetric matrix
  expect_equal(res[lower.tri(res)], res[upper.tri(res)])
  expect_equal(res[diag(res)], c(1, 1, 1))

  exp <- matrix(c(
    1, 0.018478748, -0.046885613,
    0.018478748, 1, -0.050507819,
    -0.046885613, -0.050507819, 1
  ), nrow = 3, byrow = TRUE)
  expect_equal(res, exp)


  # 1-many relationship
  # one vector to multiple vectors results in vector of results,
  # same length as rows of mat1
  res <- rsimsimd:::dist_cosine_single_mult_rs(vec1, mat1)
  expect_equal(length(res), nrow(mat1))

  exp <- c(0.022898323, -0.003896248, 0.046008395)
  expect_equal(res, exp)


  # many-many relationship
  # create a second matrix to test many-many relations
  mat2 <- do.call(rbind, lapply(seq(3), \(x) rnorm(1536)))
  expect_equal(dim(mat1), dim(mat2))

  res <- rsimsimd:::dist_cosine_mult_mult_rs(mat1, mat2)
  expect_equal(dim(res), c(3, 3))

  exp <- matrix(c(
    0.038605002, 0.019769075, 0.06331284,
    0.003389862, 0.025937742, 0.006318656,
    -0.006919892, 0.02671846, -0.011829132
  ), nrow = 3, byrow = TRUE)
  expect_equal(res, exp)
})
