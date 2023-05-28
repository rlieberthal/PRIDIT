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
#' ridit(2)
#' @export


PRIDITweight <- function(riditscores) { 
  Bijmatrix <- data.matrix(riditscores[,2:ncol(riditscores)])
  Bijtrans <- t(Bijmatrix)
  Bijsq <- Bijtrans %*% Bijmatrix
  Bijss <- diag(Bijsq)
  Bijsum <- sqrt(Bijss)
  summat <- t(matrix(Bijsum,ncol(Bijmatrix),nrow(Bijmatrix)))
  Bijnorm <- Bijmatrix/summat
  pc <- princomp(Bijmatrix, cor=TRUE)	
  maxeigval <- (pc$sdev[1])^2
  maxeigvec <- pc$load[,1]
  weightvec <- maxeigvec*pc$sdev[1]
  #	weightvec <- weightvec * -1		# PC problem - sign switch
  weightvec
}