#' Calculate the PRIDIT scores for a ridit matrix
#'
#' This function takes a matrix of data and returns the matrix transformed 
#' as ridit values.
#'
#' @param riditscores A matrix where the first column represents IDs.
#'   The IDs uniquely identify each row in the matrix.
#'   The remaining columns contain the ridit scores for each ID.
#' @param id_vector A vector of IDs.
#' @param weightvec A vector of PRIDIT weights.
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{ID}{The unique identifier for each row.}
#'     \item{ScoreMatrix}{A matrix containing PRIDIT scores for each 
#'     observation.}
#'   }
#' @export
PRIDITscore <- function(riditscores, id_vector, weightvec) {
  Bijmatrix <- data.matrix(riditscores[, 2:ncol(riditscores)])
  Bijtrans <- t(Bijmatrix)
  Bijsq <- Bijtrans %*% Bijmatrix
  Bijss <- diag(Bijsq)
  Bijsum <- sqrt(Bijss)
  summat <- t(matrix(Bijsum, ncol(Bijmatrix), nrow(Bijmatrix)))
  weightmat <- t(matrix(weightvec, ncol(Bijmatrix), nrow(Bijmatrix)))
  Bijnorm <- Bijmatrix / summat
  pc <- princomp(Bijmatrix, cor = TRUE)
  maxeigval <- (pc$sdev[1])^2
  scoremat <- (weightmat * Bijnorm) / maxeigval
  templ <- matrix(1, ncol(Bijmatrix), 1)
  scorevec <- scoremat %*% templ
  results.mat <- matrix(0, nrow(Bijmatrix), 2)
  results.mat[, 1] <- id_vector
  results.mat[, 2] <- scorevec
  results <- data.frame(Claim.ID = id_vector, PRIDITscore = scorevec)
  return(results)
}
