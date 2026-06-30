#' Extract a data portion
#'
#' @description
#' Extract a relative portion from vectors, matrices, data frames, or lists of
#' these objects.
#'
#' @param x
#' An object to be portioned.
#'
#' @param proportion \[`numeric(1)`\]\cr
#' The relative portion size as a number between `0` and `1`.
#'
#' The absolute size is rounded up with `ceiling()`.
#'
#' @param how \[`character(1)`\]\cr
#' Exactly one of:
#'
#' - `"random"` (default), select at random
#' - `"first"`, select the first elements, rows, or columns
#' - `"last"`, select the last elements, rows, or columns
#' - `"similar"`, select similar elements, rows, or columns
#' - `"dissimilar"`, select dissimilar elements, rows, or columns
#'
#' Options `"similar"` and `"dissimilar"` are based on clustering via
#' \code{\link[stats]{kmeans}} and require numeric clustering data without
#' `NA`, `NaN`, or infinite values.
#'
#' @param centers \[`integer(1)`\]\cr
#' Only relevant if `how = "similar"` or `how = "dissimilar"`. A positive whole
#' number passed to \code{\link[stats]{kmeans}} for clustering. If `centers`
#' exceeds the number of distinct values or rows, it is reduced automatically.
#'
#' @param byrow \[`logical(1)`\]\cr
#' Only relevant if `x` has two dimensions (rows and columns).
#'
#' In this case, set to `TRUE` to portion row-wise (default) or `FALSE` to
#' portion column-wise.
#'
#' @param ignore \[`integer()`\]\cr
#' Only relevant for two-dimensional `x` and `how = "similar"` or
#' `how = "dissimilar"`.
#'
#' Indices to exclude from the clustering data, but not from the returned
#' object. With `byrow = TRUE`, these are column indices. With `byrow = FALSE`,
#' these are row indices. This is useful, for example, to ignore identifier or
#' non-numeric columns while selecting similar rows.
#'
#' @param ...
#' Further arguments to be passed to or from other methods.
#'
#' @return
#' A portion of `x`, preserving attributes where possible. Vectors return
#' selected elements, matrices and data frames return selected rows or columns,
#' and lists return a list with each element portioned. The selected indices are
#' stored in the `"indices"` attribute of each returned object.
#'
#' @export
#'
#' @examples
#' # can portion vectors, matrices, data.frames, and lists of such types
#' portion(
#'   list(
#'     1:10,
#'     matrix(LETTERS[1:12], nrow = 3, ncol = 4),
#'     data.frame(a = 1:6, b = -6:-1)
#'   ),
#'   proportion = 0.5,
#'   how = "first"
#' )
#'
#' # can portion similar and dissimilar elements (based on kmeans clustering)
#' set.seed(1)
#' x <- c(1, 1, 2, 2)
#' portion(x, proportion = 0.5, how = "similar")
#' portion(x, proportion = 0.5, how = "dissimilar")
#'
#' # ignore non-numeric columns when clustering data frame rows
#' x <- data.frame(value = c(1, 1, 5, 5), group = c("a", "a", "b", "b"))
#' portion(x, proportion = 0.5, how = "similar", ignore = 2)
#'
#' # object attributes are preserved
#' x <- structure(1:10, "test_attribute" = "test")
#' x[1:5]
#' portion(x, proportion = 0.5, how = "first")

portion <- function(x, proportion, how = "random", centers = 2L, ...) {
  if (missing(proportion)) {
    stop(
      "Please provide `proportion`, a single number between 0 and 1.",
      call. = FALSE
    )
  }
  .validate_proportion(proportion)
  .validate_how(how)
  UseMethod("portion")
}

#' @export
#' @rdname portion

portion.default <- function(x, ...) {
  stop(
    sprintf(
      "No `portion()` method is available for objects with class %s.",
      paste(sprintf("`%s`", class(x)), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' @export
#' @rdname portion

portion.numeric <- function(x, proportion, how = "random", centers = 2L, ...) {
  .validate_vector(x)
  .validate_proportion(proportion)
  how <- .validate_how(how)
  n <- length(x)
  m <- .portion_size(n, proportion)
  if (m == 0L) {
    ind <- integer()
    return(structure(.extract_keep_attr(x, ind), "indices" = ind))
  }
  if (how %in% c("similar", "dissimilar")) {
    clust <- .build_cluster(x, centers)
    ind <- .cluster_indices(clust, m, similar = how == "similar")
  } else {
    ind <- .select_indices(n, m, how)
  }
  structure(.extract_keep_attr(x, ind), "indices" = ind)
}

#' @export
#' @rdname portion

portion.character <- function(x, proportion, how = "random", ...) {
  .validate_vector(x)
  .validate_proportion(proportion)
  how <- .validate_how(how)
  switch(
    how,
    random = ,
    first = ,
    last = portion.numeric(x, proportion = proportion, how = how, ...),
    similar = ,
    dissimilar = stop(
      "`how = \"similar\"` and `how = \"dissimilar\"` require numeric `x`.",
      call. = FALSE
    )
  )
}

#' @export
#' @rdname portion

portion.logical <- function(x, proportion, how = "random", centers = 2L, ...) {
  .validate_vector(x)
  .validate_proportion(proportion)
  how <- .validate_how(how)
  x <- portion.numeric(
    as.numeric(x), proportion = proportion, how = how, centers = centers, ...
  )
  `attributes<-`(as.logical(x), attributes(x))
}

#' @export
#' @rdname portion

portion.matrix <- function(
    x, proportion, how = "random", centers = 2L, byrow = TRUE,
    ignore = integer(), ...
) {
  .validate_matrix(x)
  .validate_proportion(proportion)
  how <- .validate_how(how)
  byrow <- .validate_byrow(byrow)
  n <- if (byrow) nrow(x) else ncol(x)
  m <- .portion_size(n, proportion)
  if (m == 0L) {
    ind <- integer()
    x <- if (byrow) {
      .extract_keep_attr(x, ind, )
    } else {
      .extract_keep_attr(x, , ind)
    }
    return(structure(x, "indices" = ind))
  }
  if (how %in% c("similar", "dissimilar")) {
    x_select <- if (byrow) x else t(x)
    ignore <- .validate_ignore(
      ignore,
      ncol(x_select),
      if (byrow) "column" else "row"
    )
    if (length(ignore) > 0) {
      x_select <- .extract_keep_attr(x_select, , -ignore)
    }
    cluster <- .build_cluster(x_select, centers)
    ind <- .cluster_indices(cluster, m, similar = (how == "similar"))
  } else {
    ind <- .select_indices(n, m, how)
  }
  x <- if (byrow) {
    .extract_keep_attr(x, ind, )
  } else {
    .extract_keep_attr(x, , ind)
  }
  structure(x, "indices" = ind)
}

#' @export
#' @rdname portion

portion.data.frame <- function(
    x, proportion, how = "random", centers = 2L, byrow = TRUE,
    ignore = integer(), ...
) {
  .validate_data_frame(x)
  .validate_proportion(proportion)
  how <- .validate_how(how)
  byrow <- .validate_byrow(byrow)
  n <- if (byrow) nrow(x) else ncol(x)
  m <- .portion_size(n, proportion)
  if (m == 0L) {
    ind <- integer()
    if (byrow) {
      return(structure(.extract_keep_attr(x, ind, ), "indices" = ind))
    }
    return(structure(.extract_keep_attr(x, , ind), "indices" = ind))
  }
  if (how %in% c("similar", "dissimilar")) {
    x_select <- x
    ignore <- .validate_ignore(
      ignore,
      if (byrow) ncol(x) else nrow(x),
      if (byrow) "column" else "row"
    )
    if (length(ignore) > 0) {
      x_select <- if (byrow) {
        .extract_keep_attr(x_select, , -ignore)
      } else {
        .extract_keep_attr(x_select, -ignore, )
      }
    }
    .validate_data_frame_cluster(x_select)
    x_select <- as.matrix(x_select)
    if (!byrow) {
      x_select <- t(x_select)
    }
    cluster <- .build_cluster(x_select, centers)
    ind <- .cluster_indices(cluster, m, similar = (how == "similar"))
  } else {
    ind <- .select_indices(n, m, how)
  }
  if (byrow) {
    structure(.extract_keep_attr(x, ind, ), "indices" = ind)
  } else {
    structure(.extract_keep_attr(x, , ind), "indices" = ind)
  }
}

#' @export
#' @rdname portion

portion.list <- function(x, proportion, how = "random", centers = 2L, ...) {
  if (!is.list(x)) {
    stop("`x` must be a list.", call. = FALSE)
  }
  .validate_proportion(proportion)
  .validate_how(how)
  lapply(x, portion, proportion = proportion, how = how, centers = centers, ...)
}

.validate_proportion <- function(proportion) {
  valid <- is.numeric(proportion) &&
    length(proportion) == 1L &&
    !is.na(proportion) &&
    is.finite(proportion) &&
    proportion >= 0 &&
      proportion <= 1
  if (!valid) {
    stop(
      "`proportion` must be a single finite number between 0 and 1.",
      call. = FALSE
    )
  }
  invisible(proportion)
}

.validate_how <- function(how) {
  valid_how <- c("random", "first", "last", "similar", "dissimilar")
  if (
    !is.character(how) ||
      length(how) != 1L ||
      is.na(how) ||
      !(how %in% valid_how)
  ) {
    stop(
      sprintf(
        "`how` must be one of %s.",
        paste(sprintf('"%s"', valid_how), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  how
}

.validate_vector <- function(x) {
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.", call. = FALSE)
  }
  if (!is.null(dim(x))) {
    stop(
      "`x` must be one-dimensional. Use a matrix method for matrices.",
      call. = FALSE
    )
  }
  invisible(x)
}

.validate_matrix <- function(x) {
  if (!is.matrix(x)) {
    stop("`x` must be a matrix.", call. = FALSE)
  }
  invisible(x)
}

.validate_data_frame <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }
  invisible(x)
}

.validate_byrow <- function(byrow) {
  if (!is.logical(byrow) || length(byrow) != 1L || is.na(byrow)) {
    stop("`byrow` must be either `TRUE` or `FALSE`.", call. = FALSE)
  }
  byrow
}

.validate_centers <- function(centers) {
  valid <- is.numeric(centers) &&
    length(centers) == 1L &&
    !is.na(centers) &&
    is.finite(centers) &&
    centers >= 1 &&
    centers == floor(centers) &&
      centers <= .Machine$integer.max
  if (!valid) {
    stop("`centers` must be a single positive whole number.", call. = FALSE)
  }
  as.integer(centers)
}

.validate_ignore <- function(ignore, n, what) {
  if (is.null(ignore)) {
    return(integer())
  }
  if (length(ignore) == 0L) {
    return(integer())
  }
  valid <- is.numeric(ignore) &&
    !anyNA(ignore) &&
    all(is.finite(ignore)) &&
    all(ignore == floor(ignore)) &&
    all(ignore >= 1) &&
    all(ignore <= n)
  if (!valid) {
    stop(
      sprintf(
        "`ignore` must contain whole-number %s indices between 1 and %s.",
        what,
        n
      ),
      call. = FALSE
    )
  }
  unique(as.integer(ignore))
}

.validate_data_frame_cluster <- function(x) {
  if (ncol(x) == 0L) {
    stop(
      "Clustering requires at least one non-ignored variable.",
      call. = FALSE
    )
  }
  numeric_columns <- vapply(
    x,
    function(col) is.numeric(col) || is.logical(col),
    logical(1)
  )
  if (!all(numeric_columns)) {
    stop(
      "For data frame clustering, all non-ignored columns must be numeric.",
      call. = FALSE
    )
  }
  invisible(x)
}

.portion_size <- function(n, proportion) {
  as.integer(ceiling(n * proportion))
}

.select_indices <- function(n, m, how) {
  if (m == 0L) {
    return(integer())
  }
  switch(
    how,
    random = sort(sample.int(n, m)),
    first = seq_len(m),
    last = seq.int(from = n - m + 1L, to = n)
  )
}

.build_cluster <- function(x, centers) {
  centers <- .validate_centers(centers)
  if (is.matrix(x)) {
    if (nrow(x) == 0L) {
      stop("Clustering requires at least one row.", call. = FALSE)
    }
    if (ncol(x) == 0L) {
      stop(
        "Clustering requires at least one non-ignored variable.",
        call. = FALSE
      )
    }
    centers <- min(centers, nrow(unique(x)))
  } else {
    if (length(x) == 0L) {
      stop("Clustering requires at least one value.", call. = FALSE)
    }
    centers <- min(centers, length(unique(x)))
  }
  if (!is.numeric(x)) {
    stop(
      "`how = \"similar\"` and `how = \"dissimilar\"` require numeric data.",
      call. = FALSE
    )
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop(
      "Clustering requires data without `NA`, `NaN`, or infinite values.",
      call. = FALSE
    )
  }
  stats::kmeans(x, centers = centers)$cluster
}

.cluster_indices <- function(cluster, m, similar = TRUE) {
  c <- length(unique(cluster))
  ind <- integer(0)
  if (similar) {
    i <- 1
    while (length(ind) < m && i <= c) {
      ind_i <- which(cluster == i)
      ind <- c(ind, ind_i[seq_len(min(m - length(ind), length(ind_i)))])
      i <- i + 1
    }
  } else {
    ind_cluster <- split(seq_along(cluster), cluster)
    i <- 0
    while (length(ind) < m) {
      i_mod <- i %% c + 1
      i <- i + 1
      if (length(ind_cluster[[i_mod]]) == 0) next
      ind <- c(ind, ind_cluster[[i_mod]][1])
      ind_cluster[[i_mod]] <- ind_cluster[[i_mod]][-1]
    }
  }
  sort(ind)
}

.extract_keep_attr <- function(x, ...) {
  extract <- `[`(x, ..., drop = FALSE)
  attrs <- attributes(x)
  if (!is.null(attributes(extract)$dim)) {
    attrs$dim <- attributes(extract)$dim
  } else {
    attrs$dim <- NULL
  }
  for (a in c("names", "dimnames", "row.names")) {
    if (!is.null(attributes(extract)[[a]])) {
      attrs[[a]] <- attributes(extract)[[a]]
    } else {
      attrs[[a]] <- NULL
    }
  }
  mostattributes(extract) <- attrs
  return(extract)
}
