test_that("Internal Rust functions work", {
  # test against hand-calculated result
  expect_equal(
    rsimsimd:::dist_cosine_rs(c(1, 2, 3), c(4, 5, 6)),
    0.97463173,
    tolerance = 1e-6
  )

  # simulate large vectors
  set.seed(123)
  vec1 <- rnorm(1536)
  vec2 <- rnorm(1536)

  # 1-1 relationship
  res <- rsimsimd:::dist_cosine_rs(vec1, vec2)
  expect_equal(res, -0.010641766, tolerance = 1e-6)


  # one matrix -> all combinations
  list1 <- lapply(seq(3), \(x) rnorm(1536))

  res <- rsimsimd:::dist_cosine_mat_rs(list1)
  expect_equal(dim(res), c(3, 3))
  # is a symmetric matrix
  expect_equal(res[lower.tri(res)], res[upper.tri(res)])
  expect_equal(res[diag(res)], c(1, 1, 1))

  exp <- matrix(c(
    1, 0.018478748, -0.046885613,
    0.018478748, 1, -0.050507819,
    -0.046885613, -0.050507819, 1
  ), nrow = 3, byrow = TRUE)
  expect_equal(res, exp, tolerance = 1e-6)


  # 1-many relationship
  # one vector to multiple vectors results in vector of results,
  # same length as rows of mat1
  res <- rsimsimd:::dist_cosine_single_mult_rs(vec1, list1)
  expect_equal(length(res), length(list1))

  exp <- c(0.022898323, -0.003896248, 0.046008395)
  expect_equal(res, exp, tolerance = 1e-6)


  # many-many relationship
  # create a second matrix to test many-many relations
  list2 <- lapply(seq(3), \(x) rnorm(1536))
  expect_equal(length(list1), length(list2))

  res <- rsimsimd:::dist_cosine_mult_mult_rs(list1, list2)
  expect_equal(dim(res), c(3, 3))

  exp <- matrix(c(
    -0.039135036, -0.0194967637, -0.0136295175,
    0.0003882632, 0.0167732251, -0.0112813098,
    -0.0125098021, 0.0198462835, -0.0006956676
  ), nrow = 3, byrow = TRUE)
  expect_equal(res, exp, tolerance = 1e-6)
})
