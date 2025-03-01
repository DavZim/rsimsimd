#' Calculate Cosine Similarity
#'
#' Note that this is a similarity metric (higher values indicate similar
#' vectors). The underlying simsimd rust crate calculates 1 - cosine.
#'
#' @param x a vector of embeddings, a list of embeddings, or a matrix of
#' embeddings (one row per embedding vector)
#' @param y optional second element, same structure as `x`
#'
#' @return the cosine similarity value (higher values mean similar vectors).
#' If `x` or `y` are a matrix or list while the other is a single vector,
#' the function returns a vector of the same length as the list or the number
#' of rows.
#' If `y` is NULL, a cosine similarity matrix is returned of values of `x`.
#' @export
#'
#' @examples
#' dist_cosine(c(1, 2, 3), c(4, 5, 6))
#'
#' set.seed(123)
#' N <- 10 # number of embeddings
#' emb1 <- rnorm(N)
#' emb_list_3 <- lapply(seq(3), function(x) rnorm(N))
#' emb_mat_3 <- do.call(rbind, emb_list_3)
#'
#' # we get 3 rows, one for each element,
#' # and 10 columns, one for each embedding dimension
#' dim(emb_mat_3)
#'
#' # single element - list => vector of similarities
#' dist_cosine(emb1, emb_list_3)
#' # arguments can be swapped
#' dist_cosine(emb_list_3, emb1)
#'
#' # single element - matrix => vector of similarities
#' dist_cosine(emb1, emb_mat_3)
#' # arguments can be swapped
#' dist_cosine(emb_mat_3, emb1)
#'
#' # list only results in NxN matrix
#' dist_cosine(emb_list_3)
#' # matrix only results in NxN matrix
#' dist_cosine(emb_mat_3)
#'
#' # two lists or matrices
#' emb_mat_5 <- matrix(rnorm(5 * N), ncol = N)
#'
#' # 3x5 elements results in a 3x5 matrix
#' dist_cosine(emb_mat_3, emb_mat_5)
dist_cosine <- function(x, y = NULL) {
  # there are different valid options for x and y:
  # 1: only a list of vector (or matrix) (y is NULL)
  # 2: single vector - single vector
  # 3: list of vector (or matrix) - list of vector (or matrix)
  # 4: single vector - list of vector (or matrix)
  # 5: list of vector (or matrix) - single vector

  if (is.matrix(x)) x <- split(x, seq_len(nrow(x)))
  stopifnot("x must be numeric (vector, list, or matrix)" = is_good_input(x))

  if (is.null(y)) {
    # case 1
    stopifnot("x must be a matrix or a list when y is not given" = is.list(x))
    return(dist_cosine_mat_rs(x))
  }
  if (is.matrix(y)) y <- split(y, seq_len(nrow(y)))
  stopifnot("y must be numeric (vector, list, or matrix)" = is_good_input(y))

  if (is.numeric(x) && is.numeric(y)) { # both are vectors and not list of vecs
    # case 2
    stopifnot("x and y must have the same length" = length(x) == length(y))
    return(dist_cosine_rs(x, y))
  }
  if (is.list(x) && is.list(y)) {
    # case 3
    # check that each element has the same length
    lapply(seq_along(x), function(i) {
      if (length(x[[i]]) != length(y[[1]]))
        stop("Each element of x and y must have the same length")
    })
    return(dist_cosine_mult_mult_rs(x, y))
  }
  # case 4
  # x is a vector, y is a list
  if (is.numeric(x)) return(dist_cosine_single_mult_rs(x, y))

  # case 5
  # x is a list, y is a vector
  return(dist_cosine_single_mult_rs(y, x))
}

# checks if the x or y input of dist_cosine is good
is_good_input <- function(x) {
  if (is.list(x)) return(all(sapply(x, is.numeric)))
  is.numeric(x)
}
