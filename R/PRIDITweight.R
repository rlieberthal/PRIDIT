#' Calculate the PRIDIT weights for a ridit matrix
#'
#' This function takes a ridit-scored matrix and returns PRIDIT weights for
#' each variable as a vector using Principal Component Analysis.
#'
#' @param riditscores A data frame where the first column represents IDs.
#'   The IDs uniquely identify each row in the matrix.
#'   The remaining columns contain the ridit scores for each ID.
#' @return A numeric vector containing PRIDIT weights for each variable.
#' @examples
#' # Create sample data and calculate ridit scores first
#' test_data <- data.frame(
#'   ID = c("A", "B", "C", "D", "E"),
#'   var1 = c(0.9, 0.85, 0.89, 1.0, 0.89),
#'   var2 = c(0.99, 0.92, 0.90, 1.0, 0.93),
#'   var3 = c(1.0, 0.99, 0.98, 1.0, 0.99)
#' )
#' 
#' # First calculate ridit scores
#' ridit_result <- ridit(test_data)
#' 
#' # Then calculate PRIDIT weights
#' weights <- PRIDITweight(ridit_result)
#' print(weights)
#' 
#' @references 
#' Brockett, P. L., Derrig, R. A., Golden, L. L., Levine, A., & Alpert, M. (2002).
#' Fraud classification using principal component analysis of RIDITs.
#' Journal of Risk and Insurance, 69(3), 341-371.
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
