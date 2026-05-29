#' Fit a PRIDIT model
#'
#' A single entry-point that runs the full PRIDIT pipeline---ridit scoring,
#' weight estimation, and composite scoring---and returns a classed object with
#' \code{print}, \code{summary}, \code{autoplot}, and \code{coef} methods.
#'
#' PRIDIT (Principal Component Analysis applied to RIDITs) was introduced by
#' Brockett et al. (2002) for insurance fraud detection and applied to hospital
#' quality measurement by Lieberthal (2008).  Its key properties are:
#' \enumerate{
#'   \item No parametric assumptions about the data-generating process.
#'   \item No prior knowledge of indicator direction is required; weight signs
#'         are determined entirely by the data.
#'   \item Each indicator weight is interpretable as its contribution to the
#'         dominant latent factor.
#' }
#'
#' @param data A data frame.  The first column is treated as the observation
#'   identifier; all remaining columns must be numeric indicators.
#' @param sign_correction Logical (default \code{TRUE}).  If the mean weight
#'   is negative---indicating PCA chose the opposite sign convention---all
#'   weights and scores are negated so that larger positive scores correspond
#'   to the dominant high-value direction.
#'
#' @return An object of class \code{"pridit"}, a list with components:
#' \describe{
#'   \item{\code{scores}}{Data frame with columns \code{id} and
#'     \code{PRIDITscore}, sorted descending.}
#'   \item{\code{weights}}{Named numeric vector of PRIDIT weights.}
#'   \item{\code{eigenvalue}}{Largest eigenvalue of the ridit cross-product
#'     matrix (used for score normalisation).}
#'   \item{\code{eigenvalue_ratio}}{Ratio of the first to the second eigenvalue;
#'     large values support the single-factor interpretation.}
#'   \item{\code{n}}{Number of observations.}
#'   \item{\code{p}}{Number of indicators.}
#'   \item{\code{call}}{Matched call.}
#' }
#'
#' @references
#' Brockett, P. L., Derrig, R. A., Golden, L. L., Levine, A., & Alpert, M.
#' (2002). Fraud classification using principal component analysis of RIDITs.
#' \emph{Journal of Risk and Insurance}, \strong{69}(3), 341--371.
#'
#' Lieberthal, R. D. (2008). Hospital quality: A PRIDIT approach.
#' \emph{Health Services Research}, \strong{43}(3), 988--1005.
#'
#' Lieberthal, R. D., & Comer, D. M. (2013). What are the characteristics that
#' explain hospital quality? A longitudinal PRIDIT approach.
#' \emph{Risk Management and Insurance Review}, \strong{17}(1), 17--35.
#'
#' @examples
#' dat <- data.frame(
#'   id = letters[1:10],
#'   x1 = runif(10),
#'   x2 = runif(10),
#'   x3 = runif(10)
#' )
#' fit <- pridit(dat)
#' fit
#' summary(fit)
#'
#' @seealso \code{\link{pridit_boot}}, \code{\link{pridit_longitudinal}},
#'   \code{\link{step_pridit}}
#' @export
pridit <- function(data, sign_correction = TRUE) {
  cl <- match.call()

  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (ncol(data) < 3L)
    stop("`data` must have an ID column and at least two numeric columns.",
         call. = FALSE)

  id_vec <- data[[1L]]
  n      <- nrow(data)
  p      <- ncol(data) - 1L

  # ── 1. Ridit scores ──────────────────────────────────────────────────────
  rs <- ridit(data)

  # ── 2. PCA on ridit matrix ───────────────────────────────────────────────
  bij_mat <- data.matrix(rs[, -1L, drop = FALSE])
  pc      <- stats::princomp(bij_mat, cor = TRUE)

  eigvals <- pc$sdev^2
  max_eigval    <- eigvals[1L]
  eigval_ratio  <- if (length(eigvals) >= 2L) max_eigval / eigvals[2L] else NA_real_

  # ── 3. Weights ───────────────────────────────────────────────────────────
  weight_vec <- pc$loadings[, 1L] * pc$sdev[1L]
  names(weight_vec) <- colnames(bij_mat)

  # ── 4. Scores ────────────────────────────────────────────────────────────
  col_norms  <- sqrt(colSums(bij_mat^2))
  col_norms[col_norms == 0] <- 1
  bij_norm   <- sweep(bij_mat, 2L, col_norms, "/")
  weight_mat <- matrix(weight_vec, nrow = n, ncol = p, byrow = TRUE)
  score_vec  <- rowSums(weight_mat * bij_norm) / max_eigval

  # ── 5. Sign correction ───────────────────────────────────────────────────
  if (sign_correction && mean(weight_vec) < 0) {
    weight_vec <- -weight_vec
    score_vec  <- -score_vec
  }

  # ── 6. Assemble output ───────────────────────────────────────────────────
  scores_df <- data.frame(id = id_vec, PRIDITscore = score_vec,
                          stringsAsFactors = FALSE)
  scores_df <- scores_df[order(-scores_df$PRIDITscore), ]

  structure(
    list(
      scores          = scores_df,
      weights         = weight_vec,
      eigenvalue      = max_eigval,
      eigenvalue_ratio = eigval_ratio,
      n               = n,
      p               = p,
      call            = cl
    ),
    class = "pridit"
  )
}

# ── S3 methods ───────────────────────────────────────────────────────────────

#' @export
print.pridit <- function(x, top_n = 5L, ...) {
  cat("PRIDIT model\n")
  cat(sprintf("  Observations : %d\n", x$n))
  cat(sprintf("  Indicators   : %d\n", x$p))
  cat(sprintf("  Eigenvalue 1 : %.4f  (ratio to EV2: %.2f)\n",
              x$eigenvalue, x$eigenvalue_ratio))
  cat("\nTop", top_n, "scores:\n")
  top <- utils::head(x$scores, top_n)
  print(top, row.names = FALSE, digits = 4L)
  cat("\nTop", top_n, "weights by magnitude:\n")
  wt  <- sort(abs(x$weights), decreasing = TRUE)
  wt  <- wt[seq_len(min(top_n, length(wt)))]
  wt_df <- data.frame(
    indicator = names(wt),
    weight    = x$weights[names(wt)],
    row.names = NULL
  )
  print(wt_df, digits = 4L)
  invisible(x)
}

#' @export
summary.pridit <- function(object, ...) {
  cat("PRIDIT model summary\n")
  cat("Call: "); print(object$call); cat("\n")

  cat("Score distribution:\n")
  print(summary(object$scores$PRIDITscore))

  cat("\nAll indicator weights (sorted by magnitude):\n")
  wt_order <- order(-abs(object$weights))
  wt_df <- data.frame(
    indicator = names(object$weights)[wt_order],
    weight    = round(object$weights[wt_order], 4L),
    row.names = NULL
  )
  print(wt_df)

  cat(sprintf("\nFirst eigenvalue: %.4f  |  EV1/EV2 ratio: %.2f\n",
              object$eigenvalue, object$eigenvalue_ratio))
  invisible(object)
}

#' Extract PRIDIT weights
#'
#' @param object A \code{"pridit"} object.
#' @param ... Ignored.
#' @return Named numeric vector of PRIDIT weights.
#' @export
coef.pridit <- function(object, ...) object$weights

#' Plot a PRIDIT model
#'
#' Produces a two-panel ggplot2 figure: a bar chart of the top indicator
#' weights by magnitude (left) and a histogram of the PRIDIT score distribution
#' (right).
#'
#' @param object A \code{"pridit"} object.
#' @param top_n Integer.  Number of top-weighted indicators to display.
#'   Default 20.
#' @param ... Ignored.
#' @return A \code{ggplot} object (invisibly).
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#' @export
autoplot.pridit <- function(object, top_n = 20L, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for autoplot.pridit().", call. = FALSE)
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required for autoplot.pridit().", call. = FALSE)

  wt <- object$weights
  wt_df <- data.frame(
    indicator = names(wt),
    weight    = as.numeric(wt),
    stringsAsFactors = FALSE
  )
  wt_df <- wt_df[order(-abs(wt_df$weight)), ][seq_len(min(top_n, nrow(wt_df))), ]
  wt_df$indicator <- factor(wt_df$indicator,
                            levels = wt_df$indicator[order(wt_df$weight)])

  p1 <- ggplot2::ggplot(wt_df,
         ggplot2::aes(x = .data$weight, y = .data$indicator,
                      fill = .data$weight > 0)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d6604d")) +
    ggplot2::labs(x = "PRIDIT weight", y = NULL,
                  title = paste0("Top ", top_n, " weights")) +
    ggplot2::theme_minimal(base_size = 10)

  p2 <- ggplot2::ggplot(object$scores,
         ggplot2::aes(x = .data$PRIDITscore)) +
    ggplot2::geom_histogram(bins = 40L, fill = "#4d9ab5", colour = "white") +
    ggplot2::labs(x = "PRIDIT score", y = "Count",
                  title = "Score distribution") +
    ggplot2::theme_minimal(base_size = 10)

  p1 + p2
}
