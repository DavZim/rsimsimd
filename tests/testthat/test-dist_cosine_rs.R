# reference implementation in pure R
ref_dist_cosine <- function(x, y) {
  as.vector(crossprod(x, y) / sqrt(crossprod(x) * crossprod(y)))
}
ref_dist_cosine_list <- function(x, ll) sapply(ll, ref_dist_cosine, x = x)
ref_dist_cosine_list_list <- function(list1, list2) {
  do.call(
    rbind,
    lapply(
      list1,
      function(l1) {
        sapply(
          list2,
          ref_dist_cosine,
          x = l1
        )
      }
    )
  )
}
ref_dist_cosine_mat <- function(ll) {
  mat <- matrix(1, nrow = length(ll), ncol = length(ll))
  for (i in seq_along(ll)) {
    for (j in seq_along(ll)) {
      v <- ref_dist_cosine(ll[[i]], ll[[j]])
      mat[i, j] <- v
      mat[j, i] <- v
    }
  }
  mat
}

for (i in seq(10000)) {
  v1 <- round(rnorm(10), 6)
  v2 <- round(rnorm(10), 6)

  exp <- ref_dist_cosine(v1, v2)
  got <- rsimsimd:::dist_cosine_rs(v1, v2)
  diff <- exp - got
  if (abs(diff) >= 1e-6) {
    stop(sprintf("(%s); (%s); exp %.10f; got: %.10f; diff %.10f",
                 paste(v1, collapse = ", "),
                 paste(v2, collapse = ", "),
                 exp, got, diff))
  }
}

test_that("Internal Rust functions work", {

  # test against hand-calculated result
  expect_equal(
    rsimsimd:::dist_cosine_rs(c(1, 2, 3), c(4, 5, 6)),
    ref_dist_cosine(c(1, 2, 3), c(4, 5, 6)),
    tolerance = 1e-6
  )

  # simulate large vectors
  set.seed(123)
  vec1 <- rnorm(1536)
  vec2 <- rnorm(1536)

  # 1-1 relationship
  res <- rsimsimd:::dist_cosine_rs(vec1, vec2)
  expect_equal(res, ref_dist_cosine(vec1, vec2), tolerance = 1e-6)


  # one matrix -> all combinations
  list1 <- lapply(seq(3), function(x) rnorm(1536))

  res <- rsimsimd:::dist_cosine_mat_rs(list1)
  expect_equal(dim(res), c(3, 3))
  # is a symmetric matrix
  expect_equal(res[lower.tri(res)], res[upper.tri(res)])
  expect_equal(res[diag(res)], c(1, 1, 1))

  exp <- ref_dist_cosine_mat(list1)
  expect_equal(res, exp, tolerance = 1e-6)


  # 1-many relationship
  # one vector to multiple vectors results in vector of results,
  # same length as rows of mat1
  res <- rsimsimd:::dist_cosine_single_mult_rs(vec1, list1)
  expect_equal(length(res), length(list1))

  exp <- ref_dist_cosine_list(vec1, list1)
  expect_equal(res, exp, tolerance = 1e-6)


  # many-many relationship
  # create a second matrix to test many-many relations
  list2 <- lapply(seq(3), function(x) rnorm(1536))
  expect_equal(length(list1), length(list2))

  res <- rsimsimd:::dist_cosine_mult_mult_rs(list1, list2)
  expect_equal(dim(res), c(3, 3))

  exp <- ref_dist_cosine_list_list(list1, list2)
  expect_equal(res, exp, tolerance = 1e-6)
})
