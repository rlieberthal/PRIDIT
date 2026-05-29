#' Test dataset for PRIDIT analysis
#'
#' A sample dataset containing health quality metrics for 5 healthcare providers,
#' used to demonstrate the PRIDIT scoring methodology.
#'
#' @format A data frame with 5 rows and 4 variables:
#' \describe{
#'   \item{ID}{Character. Unique identifier for each healthcare provider (A through E)}
#'   \item{Smoking_cessation}{Numeric. Smoking cessation counseling rate (0.85-1.0)}
#'   \item{ACE_Inhibitor}{Numeric. ACE inhibitor prescription rate (0.90-1.0)}
#'   \item{Proper_Antibiotic}{Numeric. Proper antibiotic usage rate (0.98-1.0)}
#' }
#' @source Synthetic data created for package examples
#' @examples
#' data(test)
#' head(test)
#'
#' # Calculate PRIDIT scores
#' ridit_scores <- ridit(test)
#' weights <- PRIDITweight(ridit_scores)
#' final_scores <- PRIDITscore(ridit_scores, test$ID, weights)
"test"
