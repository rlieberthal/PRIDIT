#' recipes step: PRIDIT composite score
#'
#' Creates a \pkg{recipes} preprocessing step that fits a PRIDIT model on the
#' training data and appends a single composite score column to any data set
#' passed to \code{bake()}.  This enables genuine out-of-sample scoring:
#' the empirical CDFs used for ridit transformation and the PCA weights are
#' estimated on the training fold only and then applied to the test fold without
#' re-fitting.
#'
#' All selected columns must be numeric.  The step does not remove the original
#' columns; use \code{step_rm()} afterwards if a clean feature set is required.
#'
#' @param recipe A \code{recipe} object.
#' @param ... One or more selector expressions passed to
#'   \code{recipes::selections()} that identify the numeric indicator columns
#'   to include in the PRIDIT model.
#' @param role For the new score column: passed to \code{add_role()}.
#'   Default \code{"predictor"}.
#' @param trained Logical.  Set automatically by \code{prep()}; do not change.
#' @param score_name Character.  Name of the new score column.
#'   Default \code{"PRIDIT_score"}.
#' @param sign_correction Logical.  Passed to \code{\link{pridit}}.
#'   Default \code{TRUE}.
#' @param ecdfs Internal.  Stored empirical CDFs from training.
#' @param weights Internal.  Stored PRIDIT weight vector from training.
#' @param max_eigval Internal.  Stored largest eigenvalue from training.
#' @param col_norms Internal.  Stored column norms from training.
#' @param skip Logical.  If \code{TRUE}, skip this step during
#'   \code{bake(new_data = NULL)}.  Default \code{FALSE}.
#' @param id Character.  Unique step identifier.
#'
#' @return An updated recipe.
#'
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' dat <- data.frame(
#'   id = letters[1:50],
#'   x1 = runif(50), x2 = runif(50), x3 = runif(50)
#' )
#'
#' rec <- recipe(~ ., data = dat) |>
#'   update_role(id, new_role = "id") |>
#'   step_pridit(x1, x2, x3)
#'
#' prepped <- prep(rec, training = dat)
#' bake(prepped, new_data = dat)
#' }
#'
#' @export
step_pridit <- function(recipe, ...,
                        role            = "predictor",
                        trained         = FALSE,
                        score_name      = "PRIDIT_score",
                        sign_correction = TRUE,
                        ecdfs           = NULL,
                        weights         = NULL,
                        max_eigval      = NULL,
                        col_norms       = NULL,
                        skip            = FALSE,
                        id              = recipes::rand_id("pridit")) {
  if (!requireNamespace("recipes", quietly = TRUE))
    stop("Package 'recipes' is required for step_pridit().", call. = FALSE)

  recipes::add_step(
    recipe,
    step_pridit_new(
      terms           = rlang::enquos(...),
      role            = role,
      trained         = trained,
      score_name      = score_name,
      sign_correction = sign_correction,
      ecdfs           = ecdfs,
      weights         = weights,
      max_eigval      = max_eigval,
      col_norms       = col_norms,
      skip            = skip,
      id              = id
    )
  )
}

step_pridit_new <- function(terms, role, trained, score_name,
                            sign_correction, ecdfs, weights,
                            max_eigval, col_norms, skip, id) {
  recipes::step(
    subclass        = "pridit",
    terms           = terms,
    role            = role,
    trained         = trained,
    score_name      = score_name,
    sign_correction = sign_correction,
    ecdfs           = ecdfs,
    weights         = weights,
    max_eigval      = max_eigval,
    col_norms       = col_norms,
    skip            = skip,
    id              = id
  )
}

#' @exportS3Method recipes::prep
prep.step_pridit <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (length(col_names) < 2L)
    stop("step_pridit() requires at least two numeric columns.", call. = FALSE)

  mat <- data.matrix(training[, col_names, drop = FALSE])
  n   <- nrow(mat)
  p   <- ncol(mat)

  # Store per-column ECDFs for out-of-sample ridit scoring
  ecdfs <- lapply(seq_len(p), function(j) stats::ecdf(mat[, j]))
  names(ecdfs) <- col_names

  # Ridit-score training data
  bij <- matrix(0.0, n, p)
  for (j in seq_len(p)) {
    fn        <- ecdfs[[j]]
    bij[, j]  <- fn(mat[, j] - 1e-10) - (1 - fn(mat[, j]))
  }

  # PCA weights
  pc         <- stats::princomp(bij, cor = TRUE)
  weight_vec <- pc$loadings[, 1L] * pc$sdev[1L]
  names(weight_vec) <- col_names

  max_eigval <- pc$sdev[1L]^2
  col_norms  <- sqrt(colSums(bij^2))
  col_norms[col_norms == 0] <- 1

  if (x$sign_correction && mean(weight_vec) < 0)
    weight_vec <- -weight_vec

  step_pridit_new(
    terms           = x$terms,
    role            = x$role,
    trained         = TRUE,
    score_name      = x$score_name,
    sign_correction = x$sign_correction,
    ecdfs           = ecdfs,
    weights         = weight_vec,
    max_eigval      = max_eigval,
    col_norms       = col_norms,
    skip            = x$skip,
    id              = x$id
  )
}

#' @exportS3Method recipes::bake
bake.step_pridit <- function(object, new_data, ...) {
  col_names <- names(object$weights)
  mat       <- data.matrix(new_data[, col_names, drop = FALSE])
  n         <- nrow(mat)
  p         <- ncol(mat)

  # Apply training ECDFs to new data
  bij <- matrix(0.0, n, p)
  for (j in seq_len(p)) {
    fn       <- object$ecdfs[[col_names[j]]]
    bij[, j] <- fn(mat[, j] - 1e-10) - (1 - fn(mat[, j]))
  }

  # Normalise using training column norms
  bij_norm   <- sweep(bij, 2L, object$col_norms, "/")
  weight_mat <- matrix(object$weights, nrow = n, ncol = p, byrow = TRUE)
  score_vec  <- rowSums(weight_mat * bij_norm) / object$max_eigval

  new_data[[object$score_name]] <- score_vec
  new_data
}

#' @export
print.step_pridit <- function(x, width = max(20, options()$width - 35), ...) {
  title <- "PRIDIT composite score from "
  recipes::print_step(x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_pridit <- function(x, ...) {
  if (x$trained) {
    data.frame(
      terms  = names(x$weights),
      weight = as.numeric(x$weights),
      id     = x$id,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      terms  = recipes::sel2char(x$terms),
      weight = NA_real_,
      id     = x$id,
      stringsAsFactors = FALSE
    )
  }
}
