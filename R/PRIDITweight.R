#' Calculate the PRIDIT weights for a ridit matrix
#'
#' This function takes a matrix of data and returns PRIDIT weights for
#' each variable as a vector.
#'
#' @param riditscores A matrix where the first column represents IDs.
#'   The IDs uniquely identify each row in the matrix.
#'   The remaining columns contain the data for each ID.
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{ID}{The unique identifier for each row.}
#'     \item{WeightMatrix}{A matrix containing PRIDIT weights for each variable.}
#'   }
#' @export
PRIDITweight <- function(riditscores) { 
  # Convert riditscores to matrix
  Bijmatrix <- data.matrix(riditscores[, 2:ncol(riditscores)])
  
  # Transpose Bijmatrix
  Bijtrans <- t(Bijmatrix)
  
  # Calculate Bijsq
  Bijsq <- Bijtrans %*% Bijmatrix
  
  # Calculate Bijss
  Bijss <- diag(Bijsq)
  
  # Calculate Bijsum
  Bijsum <- sqrt(Bijss)
  
  # Create summat matrix
  summat <- t(matrix(Bijsum, ncol(Bijmatrix), nrow(Bijmatrix)))
  
  # Normalize Bijmatrix by summat
  Bijnorm <- Bijmatrix / summat
  
  # Perform principal component analysis
  pc <- princomp(Bijmatrix, cor = TRUE)
  
  # Calculate maxeigval
  maxeigval <- (pc$sdev[1])^2
  
  # Extract the first principal component vector
  maxeigvec <- pc$load[, 1]
  
  # Calculate weightvec
  weightvec <- maxeigvec * pc$sdev[1]
  
  # Return the weightvec
  return(weightvec)
}
