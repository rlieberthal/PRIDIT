#' Calculate the ridit values for a matrix
#'
#' This function takes a matrix of data and returns the matrix transformed
#' as ridit values using the method developed by Bross (1958) and modified
#' by Brockett et al. (2002).
#'
#' @param allrawdata A data frame where the first column represents IDs.
#'   The IDs uniquely identify each row in the matrix.
#'   The remaining columns contain the numerical data for each ID.
#' @return A data frame with the first column containing IDs (named "Claim.ID")
#'   and the remaining columns containing ridit scores for each variable.
#' @examples
#' # Create sample data
#' test_data <- data.frame(
#'   ID = c("A", "B", "C", "D", "E"),
#'   var1 = c(0.9, 0.85, 0.89, 1.0, 0.89),
#'   var2 = c(0.99, 0.92, 0.90, 1.0, 0.93),
#'   var3 = c(1.0, 0.99, 0.98, 1.0, 0.99)
#' )
#'
#' # Calculate ridit scores
#' ridit_result <- ridit(test_data)
#' print(ridit_result)
#'
#' @references
#' Bross, I. D. (1958). How to use ridit analysis. Biometrics, 14(1), 18-38.
#' \doi{10.2307/2527727}
#'
#' Brockett, P. L., Derrig, R. A., Golden, L. L., Levine, A., & Alpert, M. (2002).
#' Fraud classification using principal component analysis of RIDITs.
#' Journal of Risk and Insurance, 69(3), 341-371.
#' \doi{10.1111/1539-6975.00018}
#' @export
#' @importFrom stats ecdf
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
