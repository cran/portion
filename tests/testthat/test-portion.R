test_that("can portion numeric vector", {
  x <- c(rep(1, 5), rep(3, 5))
  expect_error(
    portion(x),
    "Please provide `proportion`"
  )
  expect_error(
    portion(x, 2),
    "`proportion` must be a single finite number between 0 and 1"
  )
  expect_error(
    portion(x, 0.5, "bad"),
    "`how` must be one of"
  )
  expect_error(
    portion.numeric(x, 0.5, "bad"),
    "`how` must be one of"
  )
  expect_error(
    portion(x, 0.5, "r"),
    "`how` must be one of"
  )
  expect_length(
    portion(x, 0.5, "random"),
    5
  )
  expect_equal(
    portion(x, 0.5, "first"),
    structure(c(1, 1, 1, 1, 1), indices = 1:5)
  )
  expect_equal(
    portion(x, 0.5, "last"),
    structure(c(3, 3, 3, 3, 3), indices = 6:10)
  )
  expect_length(
    portion(x, 0.5, "similar"),
    5
  )
  expect_length(
    portion(1:100, 0.9, "dissimilar", centers = 50),
    90
  )
  expect_equal(
    portion(1:10, 0, "similar"),
    structure(integer(), indices = integer())
  )
  expect_error(
    portion(c(1, NA), 0.5, "similar"),
    "without `NA`, `NaN`, or infinite values"
  )
  expect_error(
    portion(x, 0.5, "similar", centers = 0),
    "`centers` must be a single positive whole number"
  )
})

test_that("can portion character vector", {
  x <- LETTERS[1:10]
  expect_error(
    portion.character(x, 0.5, "bad"),
    "`how` must be one of"
  )
  expect_length(
    portion(x, 0.5, "random"),
    5
  )
  expect_error(
    portion(x, 0.5, "similar"),
    "require numeric `x`"
  )
})

test_that("can portion logical vector", {
  x <- rep(c(TRUE, FALSE), 5)
  expect_error(
    portion.logical(x, 0.5, "bad"),
    "`how` must be one of"
  )
  expect_length(
    portion(x, 0.5, "similar"),
    5
  )
  expect_error(
    portion(c(TRUE, NA), 0.5, "similar"),
    "without `NA`, `NaN`, or infinite values"
  )
})

test_that("can portion matrix", {
  x <- matrix(1:24, nrow = 6)
  expect_identical(
    dim(portion(x, 0.5, "random")),
    c(3L, 4L)
  )
  expect_identical(
    dim(portion(x, 0.5, "random")),
    c(3L, 4L)
  )
  expect_identical(
    dim(portion(x, 2/3, "random", byrow = FALSE)),
    c(6L, 3L)
  )
  expect_equal(
    portion(x, 0.5, "first"),
    structure(c(1L, 2L, 3L, 7L, 8L, 9L, 13L, 14L, 15L, 19L, 20L, 21L), dim = 3:4, indices = 1:3)
  )
  expect_equal(
    portion(x, 0.5, "first", byrow = FALSE),
    structure(1:12, dim = c(6L, 2L), indices = 1:2)
  )
  expect_equal(
    portion(x, 0.5, "last"),
    structure(c(4L, 5L, 6L, 10L, 11L, 12L, 16L, 17L, 18L, 22L, 23L, 24L), dim = 3:4, indices = 4:6)
  )
  expect_equal(
    portion(x, 0.5, "last", byrow = FALSE),
    structure(13:24, dim = c(6L, 2L), indices = 3:4)
  )
  expect_identical(
    dim(portion(x, 0.5, "similar")),
    c(3L, 4L)
  )
  expect_identical(
    dim(portion(x, 0.5, "similar", byrow = FALSE)),
    c(6L, 2L)
  )
  expect_identical(
    dim(portion(x, 0.5, "similar", byrow = FALSE, ignore = 3:4)),
    c(6L, 2L)
  )
  expect_identical(
    dim(portion(x, 0.5, "dissimilar")),
    c(3L, 4L)
  )
  expect_identical(
    dim(portion(x, 0.5, "dissimilar", byrow = FALSE)),
    c(6L, 2L)
  )
  expect_equal(
    portion(matrix(1, nrow = 3, ncol = 2), 0.5, "similar"),
    structure(matrix(1, nrow = 2, ncol = 2), indices = 1:2)
  )
  expect_identical(
    dim(portion(x, 0, "similar")),
    c(0L, 4L)
  )
  expect_error(
    portion(x, 0.5, byrow = NA),
    "`byrow` must be either `TRUE` or `FALSE`"
  )
  expect_error(
    portion(x, 0.5, "similar", ignore = 5),
    "`ignore` must contain whole-number column indices"
  )
  expect_error(
    portion(x, 0.5, "similar", ignore = 1:4),
    "at least one non-ignored variable"
  )
  expect_error(
    portion(matrix(c(1, NA, 2, 3), nrow = 2), 0.5, "similar"),
    "without `NA`, `NaN`, or infinite values"
  )
})

test_that("can portion data.frame", {
  x <- cbind(as.data.frame(matrix(1:18, nrow = 6)), LETTERS[1:6])
  colnames(x) <- LETTERS[1:4]
  attr(x, "test_attribute") <- "test_attribute_value"
  expect_identical(
    dim(portion(x, 0.5, "random")),
    c(3L, 4L)
  )
  expect_identical(
    dim(portion(x, 2/3, "random", byrow = FALSE)),
    c(6L, 3L)
  )
  expect_equal(
    portion(x, 0.5, "first"),
    structure(
      list(A = 1:3, B = 7:9, C = 13:15, D = c("A", "B", "C")),
      test_attribute = "test_attribute_value",
      row.names = c(NA, 3L), class = "data.frame", indices = 1:3
    )
  )
  expect_equal(
    portion(x, 0.5, "first", byrow = FALSE),
    structure(
      list(A = 1:6, B = 7:12), class = "data.frame",
      test_attribute = "test_attribute_value",
      row.names = c(NA, 6L), indices = 1:2
    )
  )
  expect_equal(
    portion(x, 0.5, "last"),
    structure(
      list(A = 4:6, B = 10:12, C = 16:18, D = c("D", "E", "F")),
      test_attribute = "test_attribute_value", row.names = 4:6,
      class = "data.frame", indices = 4:6
    )
  )
  expect_equal(
    portion(x, 0.5, "last", byrow = FALSE),
    structure(
      list(C = 13:18, D = LETTERS[1:6]), class = "data.frame",
      test_attribute = "test_attribute_value",
      row.names = c(NA, 6L), indices = 3:4
    )
  )
  expect_identical(
    dim(portion(x, 0.5, "similar", ignore = 4)),
    c(3L, 4L)
  )
  expect_identical(
    dim(portion(x[, -4], 0.5, "similar", byrow = FALSE)),
    c(6L, 2L)
  )
  expect_identical(
    dim(portion(x[, -4], 0.5, "dissimilar")),
    c(3L, 3L)
  )
  expect_identical(
    dim(portion(x[, -4], 1/3, "dissimilar", byrow = FALSE, ignore = 4:6)),
    c(6L, 1L)
  )
  expect_error(
    portion(x, 0.5, "similar"),
    "all non-ignored columns must be numeric"
  )
  expect_identical(
    dim(portion(x, 0, "similar")),
    c(0L, 4L)
  )
  expect_error(
    portion(x, 0.5, "similar", ignore = 5),
    "`ignore` must contain whole-number column indices"
  )
  expect_error(
    portion(x, 0.5, "similar", ignore = 1:4),
    "at least one non-ignored variable"
  )
})

test_that("can portion list", {
  x <- list("vector" = 1, "matrix" = diag(2), "data.frame" = as.data.frame(diag(3)))
  expect_equal(
    portion(x, proportion = 0.5, how = "first"),
    list(
      vector = structure(1, indices = 1L),
      matrix = structure(c(1, 0), dim = 1:2, indices = 1L),
      data.frame = structure(
        list(V1 = c(1, 0), V2 = c(0, 1), V3 = c(0, 0)), row.names = 1:2,
        class = "data.frame", indices = 1:2
      )
    )
  )
})

test_that("cannot portion everything", {
  expect_error(
    portion(mean, 0.5),
    "No `portion\\(\\)` method is available"
  )
})

test_that("allows and preserves attributes", {
  x <- structure(1L:10L, "test_attribute" = "test")
  expect_identical(
    portion(x, 0.5, how = "first"),
    structure(1:5, indices = 1:5, "test_attribute" = "test")
  )
})

test_that("edge cases are validated", {
  expect_identical(
    dim(portion(matrix(1:6, nrow = 2), 0, "similar", byrow = FALSE)),
    c(2L, 0L)
  )
  expect_identical(
    dim(portion(data.frame(a = 1:3, b = 4:6), 0, "similar", byrow = FALSE)),
    c(3L, 0L)
  )
  expect_error(
    portion.list(1, 0.5),
    "`x` must be a list"
  )
  expect_error(
    portion.numeric(list(1), 0.5),
    "`x` must be an atomic vector"
  )
  expect_error(
    portion.numeric(matrix(1), 0.5),
    "`x` must be one-dimensional"
  )
  expect_error(
    portion.matrix(1:3, 0.5),
    "`x` must be a matrix"
  )
  expect_error(
    portion.data.frame(matrix(1), 0.5),
    "`x` must be a data frame"
  )
  expect_identical(
    dim(portion(matrix(1:6, nrow = 3), 0.5, "similar", ignore = NULL)),
    c(2L, 2L)
  )
  expect_identical(
    portion:::.select_indices(5L, 0L, "first"),
    integer()
  )
  expect_error(
    portion:::.build_cluster(matrix(numeric(), nrow = 0, ncol = 1), 1),
    "at least one row"
  )
  expect_error(
    portion:::.build_cluster(numeric(), 1),
    "at least one value"
  )
  expect_error(
    portion:::.build_cluster("a", 1),
    "require numeric data"
  )
})

test_that("can portion time series", {
  x <- stats::ts(1:10, start = c(2000, 3), frequency = 4)
  out <- portion(x, proportion = 0.5, how = "first")
  expect_s3_class(out, "ts")
  expect_equal(
    as.numeric(out),
    1:5
  )
  expect_equal(
    stats::tsp(out),
    stats::tsp(stats::window(x, end = c(2001, 3)))
  )
  expect_identical(
    attr(out, "indices"),
    1:5
  )
  out <- portion(x, proportion = 0.3, how = "last")
  expect_s3_class(out, "ts")
  expect_equal(
    as.numeric(out),
    8:10
  )
  expect_equal(
    stats::time(out)[1],
    stats::time(x)[8]
  )
  expect_equal(
    stats::frequency(out),
    4
  )
  set.seed(1)
  out <- portion(x, proportion = 0.5, how = "random")
  expect_false(stats::is.ts(out))
  expect_length(
    out,
    5
  )
  expect_identical(
    as.numeric(out),
    as.numeric(x)[attr(out, "indices")]
  )
  out <- portion(stats::ts(c(1, 1, 5, 1, 5, 5)), 0.5, how = "similar")
  expect_false(stats::is.ts(out))
  expect_length(
    unique(as.numeric(out)),
    1
  )
  expect_length(
    portion(x, proportion = 0, how = "first"),
    0
  )
  x <- stats::ts(cbind(a = 1:6, b = 6:1), start = 1990)
  out <- portion(x, proportion = 0.5, how = "first")
  expect_s3_class(out, "mts")
  expect_equal(
    dim(out),
    c(3, 2)
  )
  expect_equal(
    unclass(out)[, "a"],
    1:3,
    ignore_attr = TRUE
  )
  expect_equal(
    stats::start(out),
    c(1990, 1)
  )
  set.seed(1)
  out <- portion(x, proportion = 0.5)
  expect_true(is.matrix(out))
  expect_equal(
    dim(out),
    c(3, 2)
  )
})

test_that("portion size is not affected by floating-point error", {
  expect_length(
    portion(1:100, 0.14, "first"),
    14
  )
  expect_length(
    portion(1:100, 0.07, "first"),
    7
  )
  expect_length(
    portion(1:100, 0.55, "last"),
    55
  )
  expect_identical(
    dim(portion(matrix(0, nrow = 100, ncol = 2), 0.28)),
    c(28L, 2L)
  )
  for (n in c(10, 100, 1000)) {
    expect_identical(
      vapply(0:n, function(k) portion:::.portion_size(n, k / n), integer(1)),
      0:n
    )
  }
  expect_identical(
    portion:::.portion_size(10, 0.11),
    2L
  )
  expect_identical(
    portion:::.portion_size(5, 0.5),
    3L
  )
  expect_identical(
    portion:::.portion_size(10, 1e-12),
    1L
  )
  expect_identical(
    portion:::.portion_size(0, 0.5),
    0L
  )
})

test_that("can portion one-dimensional array", {
  x <- array(1:6, dim = 6, dimnames = list(letters[1:6]))
  expect_identical(
    portion(x, 0.5, "last"),
    structure(
      4:6,
      dim = 3L,
      dimnames = list(c("d", "e", "f")),
      indices = 4:6
    )
  )
  expect_error(
    portion(array(1:24, dim = 2:4), 0.5),
    "array with 3 dimensions"
  )
})

test_that("preserves names and attributes of logical vector", {
  x <- structure(
    c(a = TRUE, b = FALSE, c = TRUE, d = FALSE),
    "test_attribute" = "test"
  )
  expect_identical(
    portion(x, 0.5, "last"),
    structure(c(c = TRUE, d = FALSE), "test_attribute" = "test", indices = 3:4)
  )
  set.seed(1)
  out <- portion(c(a = TRUE, b = TRUE, c = FALSE, d = FALSE), 0.5, "similar")
  expect_type(out, "logical")
  expect_length(
    unique(out),
    1
  )
  expect_identical(
    names(out),
    letters[attr(out, "indices")]
  )
})

test_that("can cluster logical matrix and data.frame", {
  set.seed(1)
  x <- matrix(c(TRUE, TRUE, FALSE, FALSE), ncol = 1)
  expect_identical(
    dim(portion(x, 0.5, "similar")),
    c(2L, 1L)
  )
  expect_length(
    unique(as.vector(portion(x, 0.5, "similar"))),
    1
  )
  expect_identical(
    sort(as.vector(portion(x, 0.5, "dissimilar"))),
    c(FALSE, TRUE)
  )
  x <- data.frame(a = c(TRUE, TRUE, FALSE, FALSE))
  expect_length(
    unique(portion(x, 0.5, "similar")$a),
    1
  )
  expect_identical(
    sort(portion(x, 0.5, "dissimilar")$a),
    c(FALSE, TRUE)
  )
})

test_that("clustering works with as many clusters as values", {
  expect_identical(
    portion(c(1, 2), 0.5, "similar"),
    structure(1, indices = 1L)
  )
  expect_identical(
    portion(c(1, 2, 3, 4), 0.5, "dissimilar", centers = 10),
    structure(c(1, 2), indices = 1:2)
  )
  expect_identical(
    dim(portion(matrix(1:6, nrow = 3), 2 / 3, "similar", centers = 3)),
    c(2L, 2L)
  )
  expect_identical(
    portion(data.frame(a = c(1, 9)), 1, "dissimilar"),
    structure(data.frame(a = c(1, 9)), indices = 1:2)
  )
})

test_that("similar elements come from as few clusters as possible", {
  for (seed in 1:10) {
    set.seed(seed)
    expect_identical(
      portion(c(1, 2, 2), 0.5, "similar"),
      structure(c(2, 2), indices = 2:3)
    )
    expect_identical(
      portion(c(1, 5, 5, 5, 9, 9), 0.5, "similar", centers = 3),
      structure(c(5, 5, 5), indices = 2:4)
    )
  }
  expect_identical(
    portion:::.cluster_indices(c(1, 2, 2, 3, 3, 3), 4, similar = TRUE),
    c(2L, 4L, 5L, 6L)
  )
  expect_identical(
    portion:::.cluster_indices(c(1, 2, 2, 3, 3, 3), 4, similar = FALSE),
    1:4
  )
  expect_identical(
    portion:::.cluster_indices(c(2, 2, 1, 1), 2, similar = TRUE),
    3:4
  )
})

test_that("indices have no names", {
  set.seed(1)
  x <- c(a = 1, b = 1, c = 5, d = 5)
  for (how in c("random", "first", "last", "similar", "dissimilar")) {
    out <- portion(x, 0.5, how)
    expect_null(names(attr(out, "indices")))
    expect_identical(
      names(out),
      names(x)[attr(out, "indices")]
    )
  }
  x <- matrix(c(1, 1, 5, 5), ncol = 1, dimnames = list(letters[1:4], "u"))
  out <- portion(x, 0.5, "similar")
  expect_null(names(attr(out, "indices")))
  expect_identical(
    rownames(out),
    letters[attr(out, "indices")]
  )
})

test_that("can cluster columns of data.frame", {
  set.seed(1)
  x <- data.frame(a = c(1, 2, 3), b = c(1, 2, 3.1), c = c(9, 8, 7))
  expect_identical(
    names(portion(x, 0.5, "similar", byrow = FALSE)),
    c("a", "b")
  )
  expect_identical(
    names(portion(x, 0.5, "similar", byrow = FALSE, ignore = 3)),
    c("a", "b")
  )
  x$d <- c("u", "v", "w")
  expect_error(
    portion(x, 0.5, "similar", byrow = FALSE),
    "all columns must be numeric or logical vectors"
  )
  x <- data.frame(a = c(1, 2, 3))
  x$m <- matrix(1:6, nrow = 3)
  expect_error(
    portion(x, 0.5, "similar", byrow = FALSE),
    "all columns must be numeric or logical vectors"
  )
  expect_identical(
    dim(portion(x, 0.5, "first", byrow = FALSE)),
    c(3L, 1L)
  )
})

test_that("can portion multivariate time series column-wise", {
  x <- stats::ts(
    cbind(a = 1:6, b = 6:1, c = 11:16),
    start = c(1990, 2),
    frequency = 4
  )
  out <- portion(x, 1 / 3, how = "last", byrow = FALSE)
  expect_s3_class(out, "ts")
  expect_identical(
    stats::tsp(out),
    stats::tsp(x)
  )
  expect_identical(
    colnames(out),
    "c"
  )
  expect_identical(
    as.numeric(out),
    as.numeric(11:16)
  )
  expect_identical(
    attr(out, "indices"),
    3L
  )
  out <- portion(x, 2 / 3, how = "first", byrow = FALSE)
  expect_s3_class(out, "mts")
  expect_identical(
    stats::tsp(out),
    stats::tsp(x)
  )
  expect_identical(
    colnames(out),
    c("a", "b")
  )
  selections <- list()
  for (seed in 1:20) {
    set.seed(seed)
    out <- portion(x, 2 / 3, byrow = FALSE)
    expect_s3_class(out, "mts")
    expect_identical(
      stats::tsp(out),
      stats::tsp(x)
    )
    expect_identical(
      colnames(out),
      colnames(x)[attr(out, "indices")]
    )
    selections[[seed]] <- attr(out, "indices")
  }
  expect_true(list(c(1L, 3L)) %in% selections)
})

test_that("preserves attributes of list", {
  x <- structure(list(u = 1:4, v = letters[1:6]), "test_attribute" = "test")
  out <- portion(x, 0.5, "first")
  expect_identical(
    attr(out, "test_attribute"),
    "test"
  )
  expect_identical(
    names(out),
    c("u", "v")
  )
  expect_identical(
    out$u,
    structure(1:2, indices = 1:2)
  )
  expect_identical(
    out$v,
    structure(letters[1:3], indices = 1:3)
  )
})

test_that("validates list elements", {
  expect_error(
    portion(list(1:4, NULL), 0.5),
    "Element 2 of the list cannot be portioned: No `portion\\(\\)` method"
  )
  expect_error(
    portion(list(1:4, letters), 0.5, "similar"),
    "Element 2 of the list cannot be portioned: .*require numeric `x`"
  )
})
