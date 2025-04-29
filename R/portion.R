#' Extracting a data portion
#'
#' @description
#' Methods to extract portions of different objects.
#'
#' @param x
#' An object to be portioned.
#'
#' @param proportion \[`numeric(1)`\]\cr
#' The relative portion size between `0` and `1` (rounded up).
#'
#' @param how \[`character(1)`\]\cr
#' Specifying how to portion, one of:
#'
#' - `"random"` (default), portion at random
#' - `"first"`, portion to the first elements
#' - `"last"`, portion to the last elements
#' - `"similar"`, portion to similar elements
#' - `"dissimilar"`, portion to dissimilar elements
#'
#' Options `"similar"` and `"dissimilar"` are based on clustering via
#' \code{\link[stats]{kmeans}} and hence are only available for numeric `x`.
#'
#' @param centers \[`integer(1)`\]\cr
#' Only relevant if `how = "similar"` or `how = "dissimilar`.
#'
#' In this case, passed on to \code{\link[stats]{kmeans}} for clustering.
#'
#' @param byrow \[`logical(1)`\]\cr
#' Only relevant if `x` has two dimensions (rows and columns).
#'
#' In this case, set to `TRUE` to portion row-wise (default) or `FALSE` to
#' portion column-wise.
#'
#' @param ignore \[`integer()`\]\cr
#' Only relevant if `how = "similar"` or `how = "dissimilar`.
#'
#' In this case, row indices (or column indices if `byrow = FALSE`) to ignore
#' during clustering.
#'
#' @param ...
#' Further arguments to be passed to or from other methods.
#'
#' @return
#' The portioned input `x` with (row, column) indices as attributes `"indices"`.
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
#' # can portion similar elements
#' portion(c(rep(1, 5), rep(2, 5)), proportion = 0.5, how = "similar")

portion <- function(x, proportion, how = "random", centers = 2L, ...) {
  if (missing(proportion)) stop("please specify 'proportion'")
  stopifnot(
    "please set 'proportion' to a numeric between 0 and 1" =
      is.numeric(proportion) && length(proportion) == 1 && proportion <= 1 &&
      proportion >= 0
  )
  how <- match.arg(how, c("random", "first", "last", "similar", "dissimilar"))
  UseMethod("portion")
}

#' @export
#' @rdname portion

portion.numeric <- function(x, proportion, how = "random", centers = 2L, ...) {
  stopifnot("'x' must be a vector" = is.vector(x))
  n <- length(x)
  m <- ceiling(n * proportion)
  if (how %in% c("similar", "dissimilar")) {
    clust <- build_cluster(x, centers)
    ind <- cluster_indices(clust, m, similar = how == "similar")
  } else {
    ind <- switch(
      how,
      random = sort(sample.int(n, m)),
      first = seq_len(m),
      last = sort(rev(seq_len(n))[seq_len(m)]),
      stop("please use a valid method for 'how'")
    )
  }
  structure(x[ind], "indices" = ind)
}

#' @export
#' @rdname portion

portion.character <- function(x, proportion, how = "random", ...) {
  stopifnot("'x' must be a vector" = is.vector(x))
  switch(
    how,
    random = ,
    first = ,
    last = portion.numeric(x, proportion = proportion, how = how, ...),
    similar = ,
    dissimilar = stop("'x' must be numeric"),
    stop("please use a valid method for 'how'")
  )
}

#' @export
#' @rdname portion

portion.logical <- function(x, proportion, how = "random", centers = 2L, ...) {
  stopifnot("'x' must be a vector" = is.vector(x))
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
  if (!byrow) x <- t(x)
  n <- nrow(x)
  m <- ceiling(n * proportion)
  if (how %in% c("similar", "dissimilar")) {
    x_select <- if (length(ignore) > 0) x[-ignore, , drop = FALSE] else x
    cluster <- build_cluster(x_select, centers)
    ind <- cluster_indices(cluster, m, similar = (how == "similar"))
  } else {
    ind <- switch(
      how,
      random = sort(sample.int(n, m)),
      first = seq_len(m),
      last = sort(rev(seq_len(n))[seq_len(m)]),
      stop("please use a valid method for 'how'")
    )
  }
  x <- x[ind, , drop = FALSE]
  if (!byrow) x <- t(x)
  structure(x, "indices" = ind)
}

#' @export
#' @rdname portion

portion.data.frame <- function(
    x, proportion, how = "random", centers = 2L, byrow = TRUE,
    ignore = integer(), ...
) {
  if (length(ignore) > 0) {
    if (byrow) {
      x_select <- x[, -ignore, drop = FALSE]
    } else {
      x_select <- x[-ignore, , drop = FALSE]
    }
  } else {
    x_select <- x
  }
  x_portion <- portion(
    as.matrix(x_select), proportion = proportion, how = how, centers = centers,
    byrow = byrow, ignore = integer()
  )
  ind <- attr(x_portion, "indices")
  if (byrow) {
    structure(x[ind, , drop = FALSE], "indices" = ind)
  } else {
    structure(x[, ind, drop = FALSE], "indices" = ind)
  }
}

#' @export
#' @rdname portion

portion.list <- function(x, proportion, how = "random", centers = 2L, ...) {
  lapply(x, portion, proportion = proportion, how = how, centers = centers, ...)
}

build_cluster <- function(x, centers) {
  stopifnot(
    "'x' must be a vectpr or matrix" = is.vector(x) | is.matrix(x),
    "'x' must be numeric" = is.numeric(x),
    "'centers' must be a single integer" = length(centers) == 1 &&
      is.numeric(centers) && centers == as.integer(centers)
  )
  if (is.vector(x)) {
    stopifnot("length(x) must be > 0" = length(x) > 0)
    centers <- if (length(x) == 1) 1 else min(centers, length(unique(x)) - 1)
  }
  if (is.matrix(x)) {
    stopifnot("nrow(x) must be > 0" = nrow(x) > 0)
    centers <- if (nrow(x) == 1) 1 else min(centers, nrow(unique(x)) - 1)
  }
  stats::kmeans(x, centers = centers)$cluster
}

cluster_indices <- function(cluster, m, similar = TRUE) {
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
