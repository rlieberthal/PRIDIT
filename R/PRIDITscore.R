#' Calculate the PRIDIT scores for a ridit matrix
#'
#' This function takes a matrix of data and returns the matrix transformed 
#' as ridit values.
#'
#' @param riditscores A matrix where the first column represents IDs.
#'   The IDs uniquely identify each row in the matrix.
#'   The remaining columns contain the ridit scores for each ID. # Inline comment
#' @param id_vector A vector of IDs.
#' @param weightvec A vector of PRIDIT weights.
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{ID}{The unique identifier for each row.}
#'     \item{ScoreMatrix}{A matrix containing PRIDIT scores for each 
#'     observation.}
#'   }
#' @export
PRIDITscore <- function(riditscores, id_vector, weightvec)	{
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
  
  # Create results data frame
  results <- data.frame(Claim.ID=IDvector,PRIDITscore=scorevec)
  return(results)
}
