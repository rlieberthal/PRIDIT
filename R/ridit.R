#' Calculate the ridit values for a matrix
#'
#' This function takes a matrix of data and returns the matrix transformed 
#' as ridit values.
#'
#' @param allrawdata A matrix where the first column represents IDs.
#'   The IDs uniquely identify each row in the matrix.
#'   The remaining columns contain the data for each ID.
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{ID}{The unique identifier for each row.}
#'     \item{DataMatrix}{A matrix containing additional data columns.}
#'   }
#' @export
ridit <- function(allrawdata) {
  # Extract ID vector
  IDvector <- allrawdata[, 1]
  
  # Convert rawdata to matrix
  rawdata <- data.matrix(allrawdata[, 2:ncol(allrawdata)])
  
  # Initialize matrices
  Fmat <- matrix(0, nrow(rawdata), ncol(rawdata))
  Fmatmin <- matrix(0, nrow(rawdata), ncol(rawdata))
  Fmatplu <- matrix(0, nrow(rawdata), ncol(rawdata))
  bmat <- matrix(0, nrow(rawdata), ncol(rawdata))
  
  # Compute Fmatplu and Fmatmin
  for (i in 1:ncol(rawdata)) {
    Fn <- ecdf(rawdata[, i]) 
    Fmatplu[, i] <- 1 - Fn(rawdata[, i])
    Fmatmin[, i] <- Fn(rawdata[, i] - 0.001)  # Make 0.001 the smallest possible increment!
  }
  
  # Compute Bij
  Bij <- Fmatmin[, 1:ncol(rawdata)] - Fmatplu[, 1:ncol(rawdata)]
  
  # Convert Bij to a data frame
  for (j in 1:ncol(Bij)) {
    Bij.df <- data.frame(Bij[, j])
    Bij.vec <- data.matrix(Bij.df)
    Bij.vec[is.na(Bij.vec)] <- 0
    Bij[, j] <- Bij.vec
  }
  
  # Create Bij.data.frame with appropriate column names
  Bij.data.frame <- data.frame(Bij)
  colnames(Bij.data.frame) <- colnames(allrawdata[, 2:ncol(allrawdata)])
  Bij.data.frame <- data.frame(Claim.ID = IDvector, Bij.data.frame)
  
  return(Bij.data.frame)
}
