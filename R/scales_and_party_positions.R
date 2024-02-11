#' Compute Scale Score from Factor Analysis Loadings
#'
#' This function computes scale scores based on factor analysis loadings and survey data.
#' It takes a factor analysis object, survey data, and the order of the scale as inputs.
#' It returns the computed scale score for the specified scale.
#'
#' @param factanal_object An object of class 'factanal' representing the result of a factor analysis.
#' @param survey_data A data frame containing survey data where column names match the variables used in the factor analysis.
#' @param scale_order A numeric or character vector indicating the columns of the loadings to be used.
#' @return A numeric vector of scale scores for the survey data based on the specified factor analysis loadings.
#' @examples
#' # Assume factanal_object is the result of a factor analysis and df is your survey data
#' # compute_scale_score(factanal_object, df, 1) # For the first factor
#' @export
compute_scale_scores <- function(factanal_object, survey_data, scale_order) {
  # Create vector containing factor loadings
  loadings <- factanal_object$loadings[, scale_order]

  # Check if survey data contains all variables in loadings
  if (!all(names(loadings) %in% names(survey_data))) {
    missing_vars <- setdiff(names(loadings), names(survey_data))
    stop("Survey data is missing the following variables required for the scale: ", paste(missing_vars, collapse = ", "), ".")
  }

  # Convert survey_data and the wanted variables in matrix for matrix operations
  data_matrix <- as.matrix(survey_data[names(loadings),])

  # Ensure loadings are in a matrix format for matrix multiplication
  loadings_matrix <- matrix(loadings, ncol = length(loadings))

  # Matrix operation to compute scale scores
  scale_scores <- data_matrix %*% loadings_matrix

  return(scale_scores[, 1])
}