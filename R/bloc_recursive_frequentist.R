#' Fits a Gamma regression model to data and plots the estimated and real density
#'
#' @param formula A formula specifying the response and predictor variables.
#' @param link The link function to use for the model. By default, the "identity" link is used.
#' @param data A dataframe containing the variables in the formula.
#' @param plot A logical. If TRUE, the function plots the estimated and real density. If FALSE, no plot is produced.
#' @importFrom stats glm
#' @importFrom ggplot2 ggplot geom_histogram geom_density stat_function scale_fill_manual scale_color_manual labs
#' @export
gamma_model <- function(formula, link = "identity", data, plot = TRUE) {
  # Transform the response variable
  response_var <- all.vars(formula)[1]
  data[[response_var]] <- (data[[response_var]] + 1) / 2
  data[[response_var]] <- ifelse(data[[response_var]] == 0, 1e-6, data[[response_var]])
  data$response_var <- data[[response_var]]
  # Fit the gamma regression model
  model <- stats::glm(formula, data = data, family = Gamma(link = link))
  
  # Calculate shape and scale
  shape <- summary(model)$dispersion

  # Calculate the linear predictor (log link example)
  linear_predictor <- predict(model, type = "link")

  # Get the mean response (inverse link function)
  mu <- exp(linear_predictor)  # For log link

  # Calculate scale for each observation
  scale <- mean(mu) / shape

  #scale <- as.numeric(coef(model)["(Intercept)"]) / shape
  ## Check that scale is strictly positive
  #if (scale <= 0) {
  #  scale <- 1e-6
  #}
  
  # Plot if required
  if (plot) {
    p <- ggplot2::ggplot(data, aes(x = response_var)) +
      ggplot2::geom_histogram(aes(y = after_stat(density), fill = "Histogramme réel"), bins = 10, color = NA, alpha = 0.7) +
      ggplot2::geom_density(aes(color = "Densité réelle"), alpha = 0.5, fill = "#FF6666") +
      ggplot2::stat_function(fun = function(x) {
        stats::dgamma(x, shape = shape, scale = scale)
      }, aes(color = "Densité estimée (Gamma)"), size = 1) +
      ggplot2::scale_fill_manual(name = "Légende", values = c("Histogramme réel" = "grey")) +
      ggplot2::scale_color_manual(name = "Légende", values = c("Densité réelle" = "red", "Densité estimée (Gamma)" = "blue")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Comparaison de la densité réelle et estimée", x = "Response Variable", y = "Densité")
    print(p)
    model$plot <- p
  }
  
  # Add shape and scale to the model object
  model$shape <- shape
  model$scale <- scale
  
  return(model)
}


#' Fits an Inverse Gaussian regression model to data and plots the estimated and real density
#'
#' @param formula A formula specifying the response and predictor variables.
#' @param link The link function to use for the model. By default, the "identity" link is used.
#' @param data A dataframe containing the variables in the formula.
#' @param plot A logical. If TRUE, the function plots the estimated and real density. If FALSE, no plot is produced.
#' @importFrom stats glm
#' @importFrom ggplot2 ggplot geom_histogram geom_density stat_function scale_fill_manual scale_color_manual labs
#' @importFrom statmod dinvgauss
#' @export
inverse_gaussian_model <- function(formula, link = "log", data, plot = TRUE) {
  # Transform the response variable
  response_var <- all.vars(formula)[1]
  data[[response_var]] <- (data[[response_var]] + 1) / 2
  data[[response_var]] <- ifelse(data[[response_var]] == 0, 1e-6, data[[response_var]])
  data$response_var <- data[[response_var]]
  
  # Fit the inverse Gaussian regression model
  model <- stats::glm(formula, data = data, family = inverse.gaussian(link = link))
  
  # Calculate shape and scale
  shape <- summary(model)$dispersion
  
  # Calculate the linear predictor and mean response
  linear_predictor <- predict(model, type = "link")
  
  # Adjust for the chosen link function
  if (link == "log") {
    mu <- exp(linear_predictor)
  } else if (link == "identity") {
    mu <- linear_predictor
  } else if (link == "1/mu^2") {
    mu <- 1 / sqrt(linear_predictor)
  } else {
    stop("Unsupported link function. Use 'log', 'identity', or '1/mu^2'.")
  }
  
  # Calculate scale parameter
  scale <- mean(mu) / shape
  
  # Plot if required
  if (plot) {
    p <- ggplot2::ggplot(data, aes(x = response_var)) +
      ggplot2::geom_histogram(aes(y = after_stat(density), fill = "Histogramme réel"), bins = 10, color = NA, alpha = 0.7) +
      ggplot2::geom_density(aes(color = "Densité réelle"), alpha = 0.5, fill = "#FF6666") +
      ggplot2::stat_function(fun = function(x) {
        statmod::dinvgauss(x, mean = mean(mu), dispersion = shape)
      }, aes(color = "Densité estimée (Inverse Gaussian)"), size = 1) +
      ggplot2::scale_fill_manual(name = "Légende", values = c("Histogramme réel" = "grey")) +
      ggplot2::scale_color_manual(name = "Légende", values = c("Densité réelle" = "red", "Densité estimée (Inverse Gaussian)" = "blue")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Comparaison de la densité réelle et estimée", x = response_var, y = "Densité")
    print(p)
    model$plot <- p
  }
  
  # Add shape and scale to the model object
  model$shape <- shape
  model$scale <- scale
  
  return(model)
}


#' Generate diagnostic plots for a linear model
#'
#' This function creates a linear model from a given formula and data, generates
#' diagnostic plots using autoplot, and adds the plot to the model object as an
#' attribute.
#'
#' @param formula A formula specifying the response and predictor variables.
#' @param data A dataframe containing the variables in the formula.
#'
#' @return A linear model object with a diagnostic_plot attribute.
#' 
#' For more information on linear regression assumptions and diagnostics, visit:
#' \url{https://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/}
#' 
#' @importFrom ggplot2 autoplot
#' @import ggfortify
#' 
#' @examples
#' # Example with mtcars dataset
#' data(mtcars)
#' model <- lm_with_residuals(mpg ~ wt, data = mtcars)
#' 
#' @export
lm_with_residuals <- function(formula, data) {
  # Create the linear model
  model <- lm(formula, data)
  # Generate diagnostic plots using autoplot
  library(ggfortify)
  diagnostic_plot <- ggplot2::autoplot(model, which = 1:4, ncol = 2)
  # Add the plot to the model object as an attribute
  model$diagnostic_plot <- diagnostic_plot
  # Print the plot
  print(diagnostic_plot)
  # Return the model object with the plot attribute
  return(model)
}
