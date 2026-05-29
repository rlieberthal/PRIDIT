#' Compute PRIDIT weights
#'
#' Computes the PRIDIT weight vector from a ridit-scored data frame.  Weights
#' are the loadings of the first principal component of the ridit matrix,
#' scaled by the column norms of that matrix.  The sign of the weight vector
#' is arbitrary (a property of PCA); pass the result to \code{\link{pridit}}
#' rather than using this function directly if automatic sign correction is
#' desired.
#'
#' @param ridit_data A data frame returned by \code{\link{ridit}}: first column
#'   is the ID, remaining columns are ridit scores.
#' @return A named numeric vector of PRIDIT weights, one per indicator column.
#'
#' @seealso \code{\link{ridit}}, \code{\link{PRIDITscore}}, \code{\link{pridit}}
#'
#' @examples
#' dat <- data.frame(
#'   id = c("A", "B", "C", "D", "E"),
#'   x1 = c(0.90, 0.85, 0.89, 1.00, 0.89),
#'   x2 = c(0.99, 0.92, 0.90, 1.00, 0.93)
#' )
#' rs  <- ridit(dat)
#' PRIDITweight(rs)
#'
#' @export
PRIDITweight <- function(ridit_data) {
  if (!is.data.frame(ridit_data))
    stop("`ridit_data` must be a data frame returned by ridit().", call. = FALSE)

  bij_mat <- data.matrix(ridit_data[, -1L, drop = FALSE])

  if (nrow(bij_mat) <= ncol(bij_mat))
    stop("PRIDIT requires more observations than indicators (n > p).",
         call. = FALSE)

  pc         <- stats::princomp(bij_mat, cor = TRUE)
  max_eigvec <- pc$loadings[, 1L]
  weight_vec <- max_eigvec * pc$sdev[1L]

  names(weight_vec) <- colnames(bij_mat)
  weight_vec
}
