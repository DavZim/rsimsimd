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
  if (is.list(x)) x <- do.call(rbind, x)
  stopifnot("x must be numeric or a list of numerics" = is.numeric(x))

  if (is.null(y)) {
    stopifnot("x must be a matrix or a list when y is not given" = is.matrix(x))
    return(dist_cosine_mat_rs(x))
  }
  if (is.list(y)) y <- do.call(rbind, y)
  stopifnot("y must be numeric or a list of numerics" = is.numeric(y))

  if (is.vector(x) && is.vector(y)) {
    stopifnot("x and y must have the same length" = length(x) == length(y))
    return(dist_cosine_rs(x, y))
  }
  if (is.matrix(x) && is.matrix(y)) {
    stopifnot(
      "x and y must have the same number of columns" = ncol(x) == ncol(y)
    )
    return(dist_cosine_mult_mult_rs(x, y))
  }
  if (is.vector(x)) return(dist_cosine_single_mult_rs(x, y))
  # x must be a vector
  return(dist_cosine_single_mult_rs(y, x))
}
