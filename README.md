
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Extracting a Data Portion <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/loelschlaeger/portion/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/loelschlaeger/portion/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/portion)](https://CRAN.R-project.org/package=portion)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/portion/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/portion?branch=master)
<!-- badges: end -->

`{portion}` is a small `R` package that helps to extract a data portion:

1.  works for `vector`, `matrix`, `data.frame`, and `list` objects

2.  the relative portion size can be selected

3.  allows to select first, last, random, similar or dissimilar data
    points

4.  can portion either row- or column-wise

## Installation

``` r
install.packages("portion")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("loelschlaeger/portion")
```

## Example

Can portion a `vector`:

``` r
portion(c(1:5, 51:55), proportion = 0.5, how = "similar")
#> [1] 1 2 3 4 5
#> attr(,"indices")
#> [1] 1 2 3 4 5
portion(1:10, proportion = 0.4, how = "dissimilar", centers = 4)
#> [1] 1 3 5 8
#> attr(,"indices")
#> [1] 1 3 5 8
```

Can portion a `matrix`:

``` r
portion(matrix(LETTERS[1:24], nrow = 4), proportion = 0.5, how = "first")
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,] "A"  "E"  "I"  "M"  "Q"  "U" 
#> [2,] "B"  "F"  "J"  "N"  "R"  "V" 
#> attr(,"indices")
#> [1] 1 2
portion(matrix(LETTERS[1:24], nrow = 4), proportion = 0.5, how = "first", byrow = FALSE)
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
portion(as.data.frame(diag(8)), proportion = 0.3, how = "random")
#>   V1 V2 V3 V4 V5 V6 V7 V8
#> 3  0  0  1  0  0  0  0  0
#> 4  0  0  0  1  0  0  0  0
#> 5  0  0  0  0  1  0  0  0
portion(as.data.frame(diag(8)), proportion = 0.3, how = "random", byrow = FALSE)
#>   V2 V4 V8
#> 1  0  0  0
#> 2  1  0  0
#> 3  0  0  0
#> 4  0  1  0
#> 5  0  0  0
#> 6  0  0  0
#> 7  0  0  0
#> 8  0  0  1
```

Can work on a `list`:

``` r
portion(list(1:5, diag(3), data.frame(1:3, 2:4)), proportion = 0.5, how = "last")
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
