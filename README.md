---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rsimsimd

<!-- badges: start -->
[![R-CMD-check](https://github.com/DavZim/rsimsimd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DavZim/rsimsimd/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/rsimsimd)](https://CRAN.R-project.org/package=rsimsimd)
<!-- badges: end -->

`{rsimsimd}` is a light R wrapper around the rust crate [`simsimd`](https://github.com/ashvardanian/SimSIMD).
It allows efficient calculation of similarity metrics using [SIMD operations](https://en.wikipedia.org/wiki/Single_instruction,_multiple_data) 


## Installation

You can install the package like so:

``` r
# dev version
# install.packages("remotes")
# remotes::install_github("DavZim/rsimsimd")

# CRAN version
install.packages("rsimsimd")
```

## Example

This is a basic example which shows you how to solve a common problem:


``` r
library(rsimsimd)

# a simple cosine similarity calculation
dist_cosine(c(1, 2, 3),
            c(4, 5, 6))
#> [1] 0.9746317

# more realistic embedding use case
# with 1536 (OpenAI) embedding dimensions
N <- 1536
set.seed(123)
vec1 <- rnorm(N)
vec2 <- rnorm(N)

dist_cosine(vec1, vec2)
#> [1] -0.01064177

# if you have a list (or a vector) of embeddings (eg a database)
# and you want to compare a list of vectors, you can achieve this like so

# simulate a DB of 1000 embedding vectors
db <- lapply(seq(1000), function(x) rnorm(N))

# simulate a lookup of 3 embedding vectors
lookup <- lapply(seq(3), function(x) rnorm(N))

res <- dist_cosine(lookup, db)
# one row for each lookup, one column for each DB entry
dim(res)
#> [1]    3 1000
```


## Functions

- [`dist_cosine()`] Cosine Similarity Matrix
[ ] dist_dot
[ ] dist_sqeuclidean
[ ] div_jensenshannon
[ ] div_kullbackleibler

- [`get_capabilities()`] reports current hardware capabilities

[ ] GH actions
[ ] vendor dependencies
[ ] list authors
[ ] pkgdown website




## Benchmark



``` r
# alternative implementation of cosine, taken from lsa
# see also https://github.com/cran/lsa/blob/master/R/cosine.R
lsa_cosine <- function(x, y) {
  as.vector(crossprod(x, y) / sqrt(crossprod(x) * crossprod(y)))
}

set.seed(123)
N <- 1536
v1 <- rnorm(N)
v2 <- rnorm(N)

bench::mark(
  lsa_cosine(v1, v2),
  dist_cosine(v1, v2)
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 lsa_cosine(v1, v2)    6.78µs   7.21µs   118041.        0B     11.8
#> 2 dist_cosine(v1, v2)   5.69µs   7.18µs   115363.        0B     11.5

# compare 1 embedding to 1'000 embeddings
mat_1k <- matrix(rnorm(1000 * N), ncol = N)
bench::mark(
  sapply(seq(nrow(mat_1k)), function(i) lsa_cosine(v1, mat_1k[i, ])),
  dist_cosine(v1, mat_1k),
  check = FALSE # rounding errors
)
#> # A tibble: 2 × 6
#>   expression                            min  median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                         <bch:> <bch:t>     <dbl> <bch:byt>    <dbl>
#> 1 sapply(seq(nrow(mat_1k)), functio… 16.1ms 17.96ms      49.4    17.7MB     2.06
#> 2 dist_cosine(v1, mat_1k)             2.9ms  3.72ms     260.     7.86KB     0

# compare 1 embedding to 100'000 embeddings
mat_100k <- matrix(rnorm(100000 * N), ncol = N)
bench::mark(
  sapply(seq(nrow(mat_100k)), function(i) lsa_cosine(v1, mat_100k[i, ])),
  rsimsimd:::dist_cosine_single_mult_rs(v1, mat_100k),
  check = FALSE # rounding errors
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression                            min  median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                       <bch:tm> <bch:t>     <dbl> <bch:byt>    <dbl>
#> 1 sapply(seq(nrow(mat_100k)), fun…    2.17s   2.17s     0.461    1.73GB     2.77
#> 2 rsimsimd:::dist_cosine_single_m… 365.09ms 367.3ms     2.72    781.3KB     0

# 1k x 1k comparisons => 1mln comparisons
bench::mark(
  rsimsimd:::dist_cosine_mat_rs(mat_1k)
)
#> # A tibble: 1 × 6
#>   expression                             min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                           <bch> <bch:>     <dbl> <bch:byt>    <dbl>
#> 1 rsimsimd:::dist_cosine_mat_rs(mat_1… 5.29s  5.29s     0.189    7.63MB        0
```
