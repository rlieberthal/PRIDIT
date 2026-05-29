#' Bootstrap confidence intervals for PRIDIT scores and weights
#'
#' Resamples observations with replacement \code{B} times, refitting the full
#' PRIDIT pipeline on each resample.  Returns percentile confidence intervals
#' for every indicator weight and, optionally, for every observation's score.
#'
#' Because PCA sign is arbitrary, each bootstrap replicate's weight vector is
#' aligned to the original fit before aggregation: if the Pearson correlation
#' between the replicate weights and the original weights is negative, the
#' replicate is sign-flipped.
#'
#' @param fit A \code{"pridit"} object from \code{\link{pridit}}.
#' @param data The same data frame that was passed to \code{pridit()}.
#' @param B Integer.  Number of bootstrap replicates.  Default 500.
#' @param conf_level Numeric in (0, 1).  Coverage probability.  Default 0.95.
#' @param scores Logical.  If \code{TRUE} (default), also compute CIs for
#'   per-observation scores (slower for large \code{n}).
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return An object of class \code{"pridit_boot"}, a list with components:
#' \describe{
#'   \item{\code{weights_ci}}{Data frame with columns \code{indicator},
#'     \code{estimate}, \code{lower}, \code{upper}.}
#'   \item{\code{scores_ci}}{Data frame with columns \code{id},
#'     \code{estimate}, \code{lower}, \code{upper} (or \code{NULL} if
#'     \code{scores = FALSE}).}
#'   \item{\code{B}}{Number of replicates used.}
#'   \item{\code{conf_level}}{Coverage probability.}
#'   \item{\code{call}}{Matched call.}
#' }
#'
#' @examples
#' dat <- data.frame(
#'   id = letters[1:30],
#'   x1 = runif(30), x2 = runif(30), x3 = runif(30)
#' )
#' fit  <- pridit(dat)
#' boot <- pridit_boot(fit, dat, B = 100, seed = 42)
#' boot
#'
#' @seealso \code{\link{pridit}}, \code{\link{autoplot.pridit_boot}}
#' @export
pridit_boot <- function(fit, data, B = 500L, conf_level = 0.95,
                        scores = TRUE, seed = NULL) {
  cl <- match.call()
  if (!inherits(fit, "pridit"))
    stop("`fit` must be a 'pridit' object.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)

  alpha  <- (1 - conf_level) / 2
  probs  <- c(alpha, 1 - alpha)
  n      <- nrow(data)
  p      <- fit$p
  orig_w <- fit$weights

  boot_weights <- matrix(NA_real_, nrow = B, ncol = p,
                         dimnames = list(NULL, names(orig_w)))
  boot_scores  <- if (scores) matrix(NA_real_, nrow = B, ncol = n) else NULL

  for (b in seq_len(B)) {
    idx    <- sample.int(n, replace = TRUE)
    b_data <- data[idx, , drop = FALSE]

    b_fit  <- tryCatch(
      pridit(b_data, sign_correction = FALSE),
      error = function(e) NULL
    )
    if (is.null(b_fit)) next

    # Align sign to original weights
    bw <- b_fit$weights
    if (stats::cor(bw, orig_w) < 0) bw <- -bw
    boot_weights[b, ] <- bw

    if (scores) {
      # Score the *original* data with bootstrap weights (out-of-bag style)
      bs <- b_fit$scores$PRIDITscore
      if (stats::cor(bs, fit$scores$PRIDITscore[
            match(b_fit$scores$id, fit$scores$id)], use = "complete.obs") < 0)
        bs <- -bs
      boot_scores[b, ] <- bs[match(fit$scores$id, b_fit$scores$id)]
    }
  }

  # Weight CIs
  w_lower <- apply(boot_weights, 2L, stats::quantile, probs[1L], na.rm = TRUE)
  w_upper <- apply(boot_weights, 2L, stats::quantile, probs[2L], na.rm = TRUE)
  weights_ci <- data.frame(
    indicator = names(orig_w),
    estimate  = as.numeric(orig_w),
    lower     = as.numeric(w_lower),
    upper     = as.numeric(w_upper),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # Score CIs
  scores_ci <- NULL
  if (scores) {
    s_lower <- apply(boot_scores, 2L, stats::quantile, probs[1L], na.rm = TRUE)
    s_upper <- apply(boot_scores, 2L, stats::quantile, probs[2L], na.rm = TRUE)
    scores_ci <- data.frame(
      id       = fit$scores$id,
      estimate = fit$scores$PRIDITscore,
      lower    = as.numeric(s_lower),
      upper    = as.numeric(s_upper),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      weights_ci = weights_ci,
      scores_ci  = scores_ci,
      B          = B,
      conf_level = conf_level,
      call       = cl
    ),
    class = "pridit_boot"
  )
}

#' @export
print.pridit_boot <- function(x, top_n = 10L, ...) {
  cat(sprintf("PRIDIT bootstrap  (B = %d, %.0f%% CI)\n\n",
              x$B, x$conf_level * 100))
  cat("Weight confidence intervals (top", top_n, "by |estimate|):\n")
  wi <- x$weights_ci[order(-abs(x$weights_ci$estimate)), ]
  print(utils::head(wi, top_n), row.names = FALSE, digits = 3L)
  if (!is.null(x$scores_ci)) {
    cat("\nScore CIs available for", nrow(x$scores_ci),
        "observations.  Use autoplot() to visualise.\n")
  }
  invisible(x)
}

#' Plot bootstrap confidence intervals for a PRIDIT model
#'
#' Produces a point-and-range plot for indicator weight CIs and, if available,
#' a ranked-score plot with error ribbons.
#'
#' @param object A \code{"pridit_boot"} object.
#' @param top_n Integer.  Number of weights to display (by absolute estimate).
#'   Default 20.
#' @param ... Ignored.
#' @return A \code{ggplot} object (invisibly).
#' @importFrom rlang .data
#' @export
autoplot.pridit_boot <- function(object, top_n = 20L, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.", call. = FALSE)

  wi <- object$weights_ci
  wi <- wi[order(-abs(wi$estimate)), ][seq_len(min(top_n, nrow(wi))), ]
  wi$indicator <- factor(wi$indicator,
                         levels = wi$indicator[order(wi$estimate)])

  p1 <- ggplot2::ggplot(wi,
         ggplot2::aes(x = .data$estimate, y = .data$indicator,
                      xmin = .data$lower, xmax = .data$upper,
                      colour = .data$estimate > 0)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_errorbarh(height = 0.3) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "#2166ac", "FALSE" = "#d6604d"),
      guide  = "none"
    ) +
    ggplot2::labs(
      x     = "PRIDIT weight",
      y     = NULL,
      title = sprintf("Weight CIs  (B = %d, %.0f%%)",
                      object$B, object$conf_level * 100)
    ) +
    ggplot2::theme_minimal(base_size = 10)

  if (!is.null(object$scores_ci) && requireNamespace("patchwork", quietly = TRUE)) {
    sc <- object$scores_ci[order(-object$scores_ci$estimate), ]
    sc$rank <- seq_len(nrow(sc))

    p2 <- ggplot2::ggplot(sc,
           ggplot2::aes(x = .data$rank, y = .data$estimate,
                        ymin = .data$lower, ymax = .data$upper)) +
      ggplot2::geom_ribbon(fill = "#4d9ab5", alpha = 0.3) +
      ggplot2::geom_line(colour = "#4d9ab5") +
      ggplot2::labs(x = "Rank", y = "PRIDIT score",
                    title = "Score CIs by rank") +
      ggplot2::theme_minimal(base_size = 10)

    return(invisible(p1 + p2))
  }

  invisible(p1)
}
