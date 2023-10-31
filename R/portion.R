#' Extracting a data portion
#'
#' @description
#' extract a portion of data saved as a \code{vector}, \code{matrix},
#' \code{data.frame}, or \code{list}
#'
#' @param x
#' an object to be portioned
#'
#' @param proportion
#' a \code{numeric} between 0 and 1, the relative portion size
#'
#' @param how
#' a \code{character}, specifying the portion method, one of:
#' - \code{"random"} (default), portion at random
#' - \code{"first"}, portion to the first elements
#' - \code{"last"}, portion to the last elements
#' - \code{"similar"}, portion to similar elements based on clustering
#' - \code{"dissimilar"}, portion to dissimilar elements based on clustering
#'
#' @param centers
#' (only relevant if \code{how} is \code{"similar} or \code{"dissimilar)})
#' an \code{integer} (default is \code{2}), passed on to
#' \code{\link[stats]{kmeans}}
#'
#' @param byrow
#' \code{TRUE} to portion row-wise (default) or \code{FALSE} to portion
#' column-wise
#'
#' @param ignore
#' (only relevant if \code{how} is \code{"similar} or \code{"dissimilar)})
#' an \code{integer} vector of row indices (or column indices if
#' \code{byrow = FALSE}) to ignore during clustering
#'
#' @param ...
#' further arguments to be passed to or from other methods
#'
#' @return
#' the portioned input \code{x} with the (row, column) indices used
#' added as attributes \code{"indices"}
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

portion <- function(x, proportion, how, centers = 2, ...) {
  if (missing(proportion)) {
    stop("please specify 'proportion'")
  }
  stopifnot(
    "please set 'proportion' to a numeric between 0 and 1" =
      is.numeric(proportion) && length(proportion) == 1 && proportion <= 1 &&
      proportion >= 0
  )
  if (missing(how)) {
    stop("please specify 'how'")
  }
  how <- match.arg(how, c("random", "first", "last", "similar", "dissimilar"))
  UseMethod("portion")
}

#' @export
#' @rdname portion

portion.numeric <- function(x, proportion, how, centers = 2, ...) {
  n <- length(x)
  m <- ceiling(n * proportion)
  if (how == "random") {
    ind <- sort(sample.int(n, m))
  } else if (how == "first") {
    ind <- seq_len(m)
  } else if (how == "last") {
    ind <- sort(rev(seq_len(n))[1:m])
  } else if (how %in% c("similar", "dissimilar")) {
    cluster <- build_cluster(x, centers)
    ind <- cluster_indices(cluster, m, similar = how == "similar")
  } else {
    stop("please use a valid method for 'how'")
  }
  structure(x[ind], "indices" = ind)
}

#' @export
#' @rdname portion

portion.matrix <- function(
    x, proportion, how, centers = 2, byrow = TRUE, ignore = integer(), ...
) {
  if (!byrow) x <- t(x)
  n <- nrow(x)
  m <- ceiling(n * proportion)
  if (how == "random") {
    ind <- sort(sample.int(n, m))
  } else if (how == "first") {
    ind <- seq_len(m)
  } else if (how == "last") {
    ind <- sort(rev(seq_len(n))[1:m])
  } else if (how %in% c("similar", "dissimilar")) {
    if (length(ignore) > 0) {
      x_select <- x[-ignore, , drop = FALSE]
    } else {
      x_select <- x
    }
    cluster <- build_cluster(x_select, centers)
    ind <- cluster_indices(cluster, m, similar = how == "similar")
  } else {
    stop("please use a valid method for 'how'")
  }
  x <- x[ind, , drop = FALSE]
  if (!byrow) x <- t(x)
  structure(x, "indices" = ind)
}

#' @export
#' @rdname portion

portion.data.frame <- function(
    x, proportion, how, centers = 2, byrow = TRUE, ignore = integer(), ...
) {
  if (length(ignore) > 0) {
    if (byrow) {
      x_select <- x[-ignore, , drop = FALSE]
    } else {
      x_select <- x[, -ignore, drop = FALSE]
    }
  } else {
    x_select <- x
  }
  x_portion <- portion(
    as.matrix(x), proportion = proportion, how = how, centers = centers,
    byrow = byrow, ignore = integer()
  )
  ind <- attr(x_portion, "indices")
  if (byrow) {
    structure(x[ind, ], "indices" = ind)
  } else {
    structure(x[, ind], "indices" = ind)
  }
}

#' @export
#' @rdname portion

portion.list <- function(x, proportion, how, centers = 2, ...) {
  lapply(x, portion, proportion = proportion, how = how, centers = centers, ...)
}

#' build clusters
#' @keywords internal
#' @return
#' a \code{vector} of indices, indicating the allocated class

build_cluster <- function(x, centers) {
  stopifnot("'x' must be numeric" = is.numeric(x))
  stopifnot("'centers' must be a single integer" = length(centers) == 1 &&
              is.numeric(centers) && centers == as.integer(centers))
  stats::kmeans(x, centers = centers)$cluster
}

#' choose cluster indices
#' @keywords internal
#' @param cluster
#' a \code{vector} of indices, indicating the allocated class
#' @param m
#' the number of required indices
#' @param similar
#' either \code{TRUE} for similar classes, or \code{FALSE} for dissimilar
#' @return
#' a subset of \code{ind} of length \code{m}

cluster_indices <- function(cluster, m, similar = TRUE) {
  centers <- length(unique(cluster))
  ind <- integer(0)
  if (similar) {
    i <- 1
    while (length(ind) < m && i <= centers) {
      ind_i <- which(cluster == i)
      ind <- c(ind, ind_i[seq_len(min(m - length(ind), length(ind_i)))])
      i <- i + 1
    }
  } else {
    ind_cluster <- split(seq_along(cluster), cluster)
    i <- 0
    while (length(ind) < m) {
      i_mod <- i %% centers + 1
      i <- i + 1
      if (length(ind_cluster[[i_mod]]) == 0) next
      ind <- c(ind, ind_cluster[[i_mod]][1])
      ind_cluster[[i_mod]] <- ind_cluster[[i_mod]][-1]
    }
  }
  sort(ind)
}
