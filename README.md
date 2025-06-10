
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Extracting a Data Portion <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/loelschlaeger/portion/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/loelschlaeger/portion/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/portion)](https://CRAN.R-project.org/package=portion)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/portion)](https://CRAN.R-project.org/package=portion)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/portion/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/portion?branch=master)
<!-- badges: end -->

The `{portion}` `R` package offers convenient tools to extract data
portions from various objects:

1.  works for `vector`, `matrix`, `data.frame`, and `list` objects

2.  the relative portion size can be selected

3.  allows to extract first, last, random, similar or dissimilar data

4.  can portion row- and column-wise

5.  provides selected indices as an attribute

6.  preserves object attributes

## Installation

You can install the released version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("portion")
```

## Example

Can portion a `vector`:

``` r
x <- c(1, 1, 2, 2)
portion(x, proportion = 0.5, how = "similar")
#> [1] 1 1
#> attr(,"indices")
#> [1] 1 2
portion(x, proportion = 0.5, how = "dissimilar")
#> [1] 1 2
#> attr(,"indices")
#> [1] 1 3
```

Can portion a `matrix`:

``` r
x <- matrix(LETTERS[1:24], nrow = 4)
portion(x, proportion = 0.5, how = "first")
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,] "A"  "E"  "I"  "M"  "Q"  "U" 
#> [2,] "B"  "F"  "J"  "N"  "R"  "V" 
#> attr(,"indices")
#> [1] 1 2
portion(x, proportion = 0.5, how = "first", byrow = FALSE)
#>      [,1] [,2] [,3]
#> [1,] "A"  "E"  "I" 
#> [2,] "B"  "F"  "J" 
#> [3,] "C"  "G"  "K" 
#> [4,] "D"  "H"  "L" 
#> attr(,"indices")
#> [1] 1 2 3
```

Can portion a `data.frame`:

``` r
x <- as.data.frame(diag(8))
portion(x, proportion = 0.3, how = "random")
#>   V1 V2 V3 V4 V5 V6 V7 V8
#> 1  1  0  0  0  0  0  0  0
#> 3  0  0  1  0  0  0  0  0
#> 7  0  0  0  0  0  0  1  0
portion(x, proportion = 0.3, how = "random", byrow = FALSE)
#>   V1 V2 V5
#> 1  1  0  0
#> 2  0  1  0
#> 3  0  0  0
#> 4  0  0  0
#> 5  0  0  1
#> 6  0  0  0
#> 7  0  0  0
#> 8  0  0  0
```

Can work on a `list`:

``` r
x <- list(1:5, diag(3), data.frame(1:3, 2:4))
portion(x, proportion = 0.5, how = "last")
#> [[1]]
#> [1] 3 4 5
#> attr(,"indices")
#> [1] 3 4 5
#> 
#> [[2]]
#>      [,1] [,2] [,3]
#> [1,]    0    1    0
#> [2,]    0    0    1
#> attr(,"indices")
#> [1] 2 3
#> 
#> [[3]]
#>   X1.3 X2.4
#> 2    2    3
#> 3    3    4
```
