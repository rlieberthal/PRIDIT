#' Longitudinal PRIDIT analysis
#'
#' Fits a separate PRIDIT model for each time period in a panel data set and
#' summarises the stability of scores and weights across periods.  The analysis
#' follows Lieberthal & Comer (2013), who demonstrated that PRIDIT weights
#' computed on one year's Hospital Compare data predict out-of-period outcomes
#' in the following year, with cross-year weight correlations exceeding 0.99.
#'
#' @details
#' Because the PCA sign is arbitrary, each period's weight vector is aligned to
#' the \emph{first} period before computing cross-period correlations: if the
#' Pearson correlation between a replicate's weights and the first period's
#' weights is negative, the replicate is sign-flipped.
#'
#' Cross-period score correlations are computed only for the \emph{balanced}
#' panel (observations present in all periods).
#'
#' @param data A data frame in long format containing columns identified by
#'   \code{id_col}, \code{time_col}, and at least two numeric indicator columns.
#' @param id_col Character.  Name of the observation identifier column.
#' @param time_col Character.  Name of the time-period column.  Periods are
#'   processed in the order returned by \code{sort(unique(data[[time_col]]))}.
#' @param indicator_cols Character vector of indicator column names to include.
#'   If \code{NULL} (default), all numeric columns other than the ID and time
#'   columns are used.
#' @param sign_correction Logical.  Passed to \code{\link{pridit}}.
#'   Default \code{TRUE}.
#'
#' @return An object of class \code{"pridit_longitudinal"}, a list with:
#' \describe{
#'   \item{\code{fits}}{Named list of \code{"pridit"} objects, one per period.}
#'   \item{\code{weight_cors}}{Symmetric matrix of Pearson correlations between
#'     period weight vectors.}
#'   \item{\code{score_cors}}{Symmetric matrix of Spearman rank correlations
#'     between period scores on the balanced panel.}
#'   \item{\code{scores_wide}}{Data frame of scores in wide format (one column
#'     per period) for the balanced panel.}
#'   \item{\code{weights_long}}{Data frame of weights in long format with
#'     columns \code{period}, \code{indicator}, \code{weight}.}
#'   \item{\code{periods}}{Sorted vector of period labels.}
#'   \item{\code{n_balanced}}{Number of observations in the balanced panel.}
#'   \item{\code{call}}{Matched call.}
#' }
#'
#' @references
#' Lieberthal, R. D., & Comer, D. M. (2013). What are the characteristics that
#' explain hospital quality? A longitudinal PRIDIT approach.
#' \emph{Risk Management and Insurance Review}, \strong{17}(1), 17--35.
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   id   = rep(letters[1:20], times = 3),
#'   year = rep(2020:2022, each = 20),
#'   x1   = runif(60),
#'   x2   = runif(60),
#'   x3   = runif(60)
#' )
#' fit_long <- pridit_longitudinal(dat, id_col = "id", time_col = "year")
#' fit_long
#'
#' @seealso \code{\link{pridit}}, \code{\link{autoplot.pridit_longitudinal}}
#' @export
pridit_longitudinal <- function(data, id_col, time_col,
                                indicator_cols  = NULL,
                                sign_correction = TRUE) {
  cl <- match.call()

  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (!id_col   %in% names(data)) stop("`id_col` not found in data.",   call. = FALSE)
  if (!time_col %in% names(data)) stop("`time_col` not found in data.", call. = FALSE)

  periods <- sort(unique(data[[time_col]]))
  if (length(periods) < 2L)
    stop("At least two time periods are required.", call. = FALSE)

  # Determine indicator columns
  if (is.null(indicator_cols)) {
    numeric_cols   <- names(data)[vapply(data, is.numeric, logical(1L))]
    indicator_cols <- setdiff(numeric_cols, c(id_col, time_col))
  }
  if (length(indicator_cols) < 2L)
    stop("At least two indicator columns are required.", call. = FALSE)

  # ── Fit per-period models ─────────────────────────────────────────────────
  fits <- vector("list", length(periods))
  names(fits) <- as.character(periods)

  for (i in seq_along(periods)) {
    sub  <- data[data[[time_col]] == periods[i], c(id_col, indicator_cols),
                 drop = FALSE]
    sub  <- sub[stats::complete.cases(sub), , drop = FALSE]
    fits[[i]] <- pridit(sub, sign_correction = sign_correction)
  }

  # ── Align signs to first period ───────────────────────────────────────────
  ref_w <- fits[[1L]]$weights
  for (i in seq_along(fits)[-1L]) {
    if (stats::cor(fits[[i]]$weights, ref_w) < 0)
      fits[[i]]$weights <- -fits[[i]]$weights
  }

  # ── Cross-period weight correlations ─────────────────────────────────────
  K        <- length(periods)
  wt_names <- as.character(periods)
  wt_cors  <- matrix(NA_real_, K, K, dimnames = list(wt_names, wt_names))
  for (i in seq_len(K)) {
    for (j in seq_len(K)) {
      wi <- fits[[i]]$weights
      wj <- fits[[j]]$weights
      common <- intersect(names(wi), names(wj))
      if (length(common) >= 2L)
        wt_cors[i, j] <- stats::cor(wi[common], wj[common])
    }
  }

  # ── Balanced panel scores ─────────────────────────────────────────────────
  id_lists  <- lapply(fits, function(f) f$scores$id)
  common_ids <- Reduce(intersect, id_lists)

  scores_wide <- data.frame(id = common_ids, stringsAsFactors = FALSE)
  for (i in seq_along(fits)) {
    sc <- fits[[i]]$scores
    sc <- sc[match(common_ids, sc$id), ]
    scores_wide[[wt_names[i]]] <- sc$PRIDITscore
  }

  # ── Cross-period score correlations (Spearman) ────────────────────────────
  sc_mat   <- data.matrix(scores_wide[, -1L, drop = FALSE])
  sc_cors  <- matrix(NA_real_, K, K, dimnames = list(wt_names, wt_names))
  for (i in seq_len(K)) {
    for (j in seq_len(K)) {
      sc_cors[i, j] <- stats::cor(sc_mat[, i], sc_mat[, j],
                                  method = "spearman", use = "complete.obs")
    }
  }

  # ── Weights in long format ────────────────────────────────────────────────
  weights_long <- do.call(rbind, lapply(seq_along(fits), function(i) {
    data.frame(
      period    = wt_names[i],
      indicator = names(fits[[i]]$weights),
      weight    = as.numeric(fits[[i]]$weights),
      stringsAsFactors = FALSE
    )
  }))

  structure(
    list(
      fits          = fits,
      weight_cors   = wt_cors,
      score_cors    = sc_cors,
      scores_wide   = scores_wide,
      weights_long  = weights_long,
      periods       = periods,
      n_balanced    = length(common_ids),
      call          = cl
    ),
    class = "pridit_longitudinal"
  )
}

#' @export
print.pridit_longitudinal <- function(x, ...) {
  cat("Longitudinal PRIDIT\n")
  cat(sprintf("  Periods    : %s\n", paste(x$periods, collapse = ", ")))
  cat(sprintf("  Balanced n : %d\n\n", x$n_balanced))

  cat("Weight correlations across periods:\n")
  print(round(x$weight_cors, 3L))

  cat("\nSpearman score correlations (balanced panel):\n")
  print(round(x$score_cors, 3L))
  invisible(x)
}

#' Plot a longitudinal PRIDIT analysis
#'
#' Produces two panels: (left) a heatmap of cross-period Spearman score
#' correlations and (right) a line plot of per-indicator weight trajectories
#' across periods.
#'
#' @param object A \code{"pridit_longitudinal"} object.
#' @param top_n Integer.  Number of indicators to show in the weight trajectory
#'   panel (by mean absolute weight across periods).  Default 10.
#' @param ... Ignored.
#' @return A \code{ggplot} object (invisibly).
#' @export
autoplot.pridit_longitudinal <- function(object, top_n = 10L, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.", call. = FALSE)

  # Heatmap of score correlations
  sc_long <- as.data.frame(as.table(object$score_cors),
                            stringsAsFactors = FALSE)
  names(sc_long) <- c("Period1", "Period2", "rho")

  p1 <- ggplot2::ggplot(sc_long,
         ggplot2::aes(x = .data$Period1, y = .data$Period2,
                      fill = .data$rho)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$rho, 2L)),
                       size = 3L) +
    ggplot2::scale_fill_distiller(palette = "Blues", direction = 1L,
                                  limits = c(0, 1)) +
    ggplot2::labs(title = "Cross-period score correlations (Spearman)",
                  x = NULL, y = NULL, fill = expression(rho)) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Weight trajectories for top indicators
  mean_abs <- tapply(abs(object$weights_long$weight),
                     object$weights_long$indicator, mean)
  top_inds <- names(sort(mean_abs, decreasing = TRUE))[
    seq_len(min(top_n, length(mean_abs)))]

  wl_sub <- object$weights_long[object$weights_long$indicator %in% top_inds, ]

  p2 <- ggplot2::ggplot(wl_sub,
         ggplot2::aes(x = .data$period, y = .data$weight,
                      group = .data$indicator, colour = .data$indicator)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = paste0("Top ", top_n, " weight trajectories"),
                  x = "Period", y = "PRIDIT weight", colour = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "right")

  if (requireNamespace("patchwork", quietly = TRUE))
    return(invisible(p1 + p2))

  invisible(p1)
}
