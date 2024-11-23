#' Fits a Gamma regression model to data and plots the estimated and real density
#'
#' @param formula A formula specifying the response and predictor variables.
#' @param data A dataframe containing the variables in the formula.
#' @param plot A logical. If TRUE, the function plots the estimated and real density. If FALSE, no plot is produced.
#' @importFrom stats glm
#' @importFrom ggplot2 ggplot geom_histogram geom_density stat_function scale_fill_manual scale_color_manual labs
#' @export
gamma_model <- function(formula, data, plot = TRUE) {
  # Transform the response variable
  response_var <- all.vars(formula)[1]
  data[[response_var]] <- (data[[response_var]] + 1) / 2
  data[[response_var]] <- ifelse(data[[response_var]] == 0, 1e-6, data[[response_var]])
  data$response_var <- data[[response_var]]
  # Fit the gamma regression model
  model <- stats::glm(formula, data = data, family = Gamma(link = "identity"))
  
  # Calculate shape and scale
  shape <- summary(model)$dispersion
  scale <- as.numeric(coef(model)["(Intercept)"]) / shape
  
  # Plot if required
  if (plot) {
    p <- ggplot2::ggplot(data, aes(x = response_var)) +
      ggplot2::geom_histogram(aes(y = after_stat(density), fill = "Histogramme réel"), bins = 10, color = NA, alpha = 0.7) +
      ggplot2::geom_density(aes(color = "Densité réelle"), alpha = 0.5, fill = "#FF6666") +
      ggplot2::stat_function(fun = stats::dgamma, args = list(shape = shape, scale = scale), aes(color = "Densité estimée (Gamma)")) +
      ggplot2::scale_fill_manual(name = "Légende", values = c("Histogramme réel" = "grey")) +
      ggplot2::scale_color_manual(name = "Légende", values = c("Densité réelle" = "red", "Densité estimée (Gamma)" = "blue")) +
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
