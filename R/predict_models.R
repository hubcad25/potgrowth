#' Find the Mode of a Factor Variable
#'
#' This function calculates the mode of a factor variable, i.e., the level
#' that appears most frequently in the factor. It returns the mode or modes if
#' there are ties for the most frequent level.
#'
#' @param x A factor variable. The function will stop and return an error if 'x' is not a factor.
#' @return A vector containing the mode(s) of the factor. If there are multiple modes,
#' all of them are returned.
#' @examples
#' factor_variable <- factor(c("red", "blue", "red", "green", "blue", "red"))
#' mode(factor_variable)
#' @export
mode <- function(x) {
  # Ensure x is a factor
  if (!is.factor(x)) {
    stop("x must be a factor")
  }
  # Use table to get frequencies of each level
  levels_freq <- table(x)
  # Identify the most frequent level
  mode <- names(levels_freq)[levels_freq == max(levels_freq)]
  # Return the mode; if there are multiple modes, return them all
  return(mode)
}
