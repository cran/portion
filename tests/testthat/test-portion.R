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
