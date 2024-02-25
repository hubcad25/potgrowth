#' Softmax Function
#'
#' Computes the softmax of a given array of values, a transformation often used
#' in the context of machine learning and statistics, especially for converting
#' logits to probabilities. This implementation includes numerical stabilization
#' by shifting the inputs by their maximum value.
#'
#' @param array_of_values Numeric vector of input values (logits).
#'
#' @return A numeric vector of the same length as \code{array_of_values}, where each
#' element is the softmax of the corresponding input value, representing probabilities
#' that sum to 1.
#'
#' @examples
#' logits <- c(1, 2, 3, 4, 5)
#' probabilities <- softmax(logits)
#' print(probabilities)
#' sum(probabilities)  # Should be very close to 1
#'
#' @export
softmax <- function(array_of_values){
  n.par <- length(array_of_values)
  par1 <- sort(array_of_values, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk)))
  }
  val <- exp(array_of_values - Lk)
  return(val)
}
