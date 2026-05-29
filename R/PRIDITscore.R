#' Compute PRIDIT scores
#'
#' Applies a vector of PRIDIT weights to a ridit-scored data frame and returns
#' a composite score in \eqn{(-1, 1)} for each observation.  The score is
#' normalised by the largest eigenvalue so that the mean score is zero by
#' construction.
#'
#' @param ridit_data A data frame returned by \code{\link{ridit}}: first column
#'   is the ID, remaining columns are ridit scores.
#' @param id_vector A vector of observation identifiers (same length and order
#'   as the rows of \code{ridit_data}).
#' @param weight_vec A named numeric vector of PRIDIT weights returned by
#'   \code{\link{PRIDITweight}}.
#' @return A data frame with columns \code{id} and \code{PRIDITscore}.
#'
#' @seealso \code{\link{ridit}}, \code{\link{PRIDITweight}}, \code{\link{pridit}}
#'
#' @examples
#' dat <- data.frame(
#'   id = c("A", "B", "C", "D", "E"),
#'   x1 = c(0.90, 0.85, 0.89, 1.00, 0.89),
#'   x2 = c(0.99, 0.92, 0.90, 1.00, 0.93)
#' )
#' rs  <- ridit(dat)
#' wts <- PRIDITweight(rs)
#' PRIDITscore(rs, dat$id, wts)
#'
#' @export
PRIDITscore <- function(ridit_data, id_vector, weight_vec) {
  if (!is.data.frame(ridit_data))
    stop("`ridit_data` must be a data frame returned by ridit().", call. = FALSE)
  if (length(id_vector) != nrow(ridit_data))
    stop("`id_vector` must have the same length as nrow(ridit_data).",
         call. = FALSE)

  bij_mat  <- data.matrix(ridit_data[, -1L, drop = FALSE])
  p        <- ncol(bij_mat)

  # Column norms for normalisation
  col_norms <- sqrt(colSums(bij_mat^2))
  col_norms[col_norms == 0] <- 1          # guard against zero-variance cols
  bij_norm  <- sweep(bij_mat, 2L, col_norms, "/")

  # Largest eigenvalue (same PCA used in PRIDITweight)
  pc        <- stats::princomp(bij_mat, cor = TRUE)
  max_eigval <- pc$sdev[1L]^2

  weight_mat <- matrix(weight_vec, nrow = nrow(bij_mat), ncol = p,
                       byrow = TRUE)
  score_vec  <- rowSums(weight_mat * bij_norm) / max_eigval

  data.frame(id = id_vector, PRIDITscore = score_vec,
             stringsAsFactors = FALSE)
}
