#' Compute ridit scores
#'
#' Transforms a data frame of numeric indicators into ridit scores on the
#' interval \eqn{(-1, 1)} using the empirical cumulative distribution of each
#' column across the reference population.  A score of zero indicates a value
#' exactly at the median; positive scores indicate above-median values.
#'
#' The ridit score for observation \eqn{i} on indicator \eqn{j} is
#' \deqn{B_{ij} = F_j(x_{ij} - \varepsilon) - [1 - F_j(x_{ij})]}
#' where \eqn{F_j} is the empirical CDF of column \eqn{j} and \eqn{\varepsilon}
#' is a small constant that makes the lower CDF strictly left-continuous.
#' This formulation is robust to ties and requires no parametric assumptions.
#'
#' Categorical indicators should be expanded into binary dummy columns before
#' calling \code{ridit()}; each dummy then receives its own ridit transformation
#' and PRIDIT weight, with sign determined by the data rather than by the
#' analyst.
#'
#' @param data A data frame whose first column is an ID and whose remaining
#'   columns are numeric indicators.
#' @return A data frame of the same shape as \code{data} with numeric columns
#'   replaced by their ridit scores.  The ID column is preserved as-is.
#'
#' @references
#' Bross, I. D. J. (1958). How to use ridit analysis. \emph{Biometrics},
#' \strong{14}(1), 18--38.
#'
#' Brockett, P. L., Derrig, R. A., Golden, L. L., Levine, A., & Alpert, M.
#' (2002). Fraud classification using principal component analysis of RIDITs.
#' \emph{Journal of Risk and Insurance}, \strong{69}(3), 341--371.
#'
#' @examples
#' dat <- data.frame(
#'   id = c("A", "B", "C", "D", "E"),
#'   x1 = c(0.90, 0.85, 0.89, 1.00, 0.89),
#'   x2 = c(0.99, 0.92, 0.90, 1.00, 0.93)
#' )
#' ridit(dat)
#'
#' @export
ridit <- function(data) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (ncol(data) < 2L)
    stop("`data` must have at least one ID column and one numeric column.",
         call. = FALSE)

  id_col   <- data[[1L]]
  raw_mat  <- data.matrix(data[, -1L, drop = FALSE])
  col_nms  <- colnames(raw_mat)
  n        <- nrow(raw_mat)
  p        <- ncol(raw_mat)

  bij <- matrix(0.0, nrow = n, ncol = p)

  for (j in seq_len(p)) {
    x    <- raw_mat[, j]
    fn   <- stats::ecdf(x)
    f_lo <- fn(x - 1e-10)   # P(X < x)  -- strictly below
    f_hi <- fn(x)            # P(X <= x) -- at or below
    bij[, j] <- f_lo - (1 - f_hi)   # ranges in (-1, 1)
  }

  out           <- as.data.frame(bij)
  colnames(out) <- col_nms
  out           <- cbind(data[1L], out)
  out
}
