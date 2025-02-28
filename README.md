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
n_dimensions <- 1536
v1 <- rnorm(N)
v2 <- rnorm(N)

bench::mark(
  lsa_cosine(v1, v2),
  dist_cosine(v1, v2)
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 lsa_cosine(v1, v2)    6.79µs   7.47µs   123775.        0B      0  
#> 2 dist_cosine(v1, v2)   6.99µs   8.89µs   107573.        0B     10.8

# compare 1 embedding to 1'000 embeddings
ll_1k <- lapply(seq(1000), function(i) rnorm(n_dimensions))

bench::mark(
  sapply(ll_1k, function(ll) lsa_cosine(v1, ll)),
  dist_cosine(v1, ll_1k),
  check = FALSE # rounding errors
)
#> # A tibble: 2 × 6
#>   expression                             min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                          <bch:> <bch:>     <dbl> <bch:byt>    <dbl>
#> 1 sapply(ll_1k, function(ll) lsa_cos… 8.12ms 8.34ms      117.    31.7KB     2.05
#> 2 dist_cosine(v1, ll_1k)              1.16ms 1.37ms      655.    35.6KB     2.04

# compare 1 embedding to 100'000 embeddings
ll_100k <- lapply(seq(100000), function(i) rnorm(n_dimensions))

bench::mark(
  sapply(ll_100k, function(ll) lsa_cosine(v1, ll)),
  dist_cosine(v1, ll_100k),
  check = FALSE # rounding errors
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression                             min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                           <bch> <bch:>     <dbl> <bch:byt>    <dbl>
#> 1 sapply(ll_100k, function(ll) lsa_co… 933ms  933ms      1.07    3.29MB     1.07
#> 2 dist_cosine(v1, ll_100k)             165ms  165ms      5.70    3.67MB     0

# 1k x 1k comparisons => 1mln comparisons
bench::mark(
  dist_cosine(ll_1k)
)
#> # A tibble: 1 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 dist_cosine(ll_1k)    294ms    305ms      3.28    7.66MB        0
```
