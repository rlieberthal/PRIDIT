#' Calculate the PRIDIT scores for a ridit matrix
#'
#' This function takes ridit scores and PRIDIT weights to calculate final
#' PRIDIT scores for each observation.
#'
#' @param riditscores A data frame where the first column represents IDs.
#'   The IDs uniquely identify each row in the matrix.
#'   The remaining columns contain the ridit scores for each ID.
#' @param id_vector A vector of IDs corresponding to the observations.
#' @param weightvec A numeric vector of PRIDIT weights (from PRIDITweight function).
#' @return A data frame with two columns: "Claim.ID" containing the IDs and
#'   "PRIDITscore" containing the calculated PRIDIT scores (ranging from -1 to 1).
#' @examples
#' # Complete workflow example
#' test_data <- data.frame(
#'   ID = c("A", "B", "C", "D", "E"),
#'   var1 = c(0.9, 0.85, 0.89, 1.0, 0.89),
#'   var2 = c(0.99, 0.92, 0.90, 1.0, 0.93),
#'   var3 = c(1.0, 0.99, 0.98, 1.0, 0.99)
#' )
#'
#' # Step 1: Calculate ridit scores
#' ridit_result <- ridit(test_data)
#'
#' # Step 2: Calculate PRIDIT weights
#' weights <- PRIDITweight(ridit_result)
#'
#' # Step 3: Calculate final PRIDIT scores
#' final_scores <- PRIDITscore(ridit_result, test_data$ID, weights)
#' print(final_scores)
#'
#' @references
#' Brockett, P. L., Derrig, R. A., Golden, L. L., Levine, A., & Alpert, M. (2002).
#' Fraud classification using principal component analysis of RIDITs.
#' Journal of Risk and Insurance, 69(3), 341-371.
#' @export
#' @importFrom stats princomp
PRIDITscore <- function(riditscores, id_vector, weightvec) {
  # riditscores should have ID in the first column
  # Convert riditscores to matrix
  Bijmatrix <- data.matrix(riditscores[,2:ncol(riditscores)])

  # Transpose Bijmatrix
  Bijtrans <- t(Bijmatrix)

  # Calculate Bijsq
  Bijsq <- Bijtrans %*% Bijmatrix

  # Calculate Bijss
  Bijss <- diag(Bijsq)

  # Calculate Bijsum
  Bijsum <- sqrt(Bijss)

  # Create summat matrix
  summat <- t(matrix(Bijsum,ncol(Bijmatrix),nrow(Bijmatrix)))

  # Create weightmat matrix
  weightmat <- t(matrix(weightvec,ncol(Bijmatrix),nrow(Bijmatrix)))

  # Normalize Bijmatrix by summat
  Bijnorm <- Bijmatrix/summat

  # Perform principal component analysis
  pc <- princomp(Bijmatrix, cor=TRUE)

  # Calculate maxeigval
  maxeigval <- (pc$sdev[1])^2

  # Calculate scoremat
  scoremat <- (weightmat*Bijnorm)/maxeigval

  # Create templ matrix
  templ <- matrix(1,ncol(Bijmatrix),1)

  # Calculate scorevec
  scorevec <- scoremat %*% templ

  # Create results.mat matrix
  results.mat <- matrix(0,nrow(Bijmatrix),2)
  results.mat[,1] <- id_vector
  results.mat[,2] <- scorevec

  # Create results data frame - FIXED: changed IDvector to id_vector
  results <- data.frame(Claim.ID=id_vector, PRIDITscore=scorevec)
  return(results)
}
