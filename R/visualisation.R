#' Create Custom Geom for IRC Visualization
#'
#' This function creates a custom geom designed specifically for visualizing
#' the IRC along with its confidence intervals and other related estimates.
#' It adds multiple `geom_linerange` layers to a ggplot object, using color as an aesthetic.
#'
#' @param mapping Set of aesthetic mappings created by `aes()` or `aes_()`.
#'   Expected aesthetics are `conf_low_irc`, `conf_high_irc`, `estimate_irc`,
#'   `estimate_vote`, `conf_low_vote`, `conf_high_vote`, and `color`.
#' @param data Optional dataset to use for the layer.
#' @param ... Additional parameters for fine-tuning the `geom_linerange` components,
#'   such as `size` and `alpha`.
#' @return A ggplot2 layer.
#' @examples
#' df <- data.frame(
#'   conf_low_irc = rnorm(10, 0.1, 0.05),
#'   conf_high_irc = rnorm(10, 0.2, 0.05),
#'   estimate_irc = rnorm(10, 0.15, 0.05),
#'   estimate_vote = runif(10, 0.4, 0.6),
#'   conf_low_vote = runif(10, 0.35, 0.45),
#'   conf_high_vote = runif(10, 0.55, 0.65),
#'   color = rep(c("red", "blue"), each = 5)
#' )
#' ggplot(df, aes(conf_low_irc = conf_low_irc, conf_high_irc = conf_high_irc,
#'                estimate_irc = estimate_irc, estimate_vote = estimate_vote,
#'                conf_low_vote = conf_low_vote, conf_high_vote = conf_high_vote,
#'                color = color)) +
#'   geom_irc()
#' @export
geom_irc <- function(mapping = NULL, data = NULL, ...) {
  # Check for necessary aesthetics and report missing ones
  required_aes <- c("conf_low_irc", "conf_high_irc", "estimate_irc", "estimate_vote", "conf_low_vote", "conf_high_vote", "color")
  if (!is.null(mapping)) {
    provided_aes <- names(mapping$aes)
    missing_aes <- setdiff(required_aes, provided_aes)
    if (length(missing_aes) > 0) {
      stop("Missing required aesthetics: ", paste(missing_aes, collapse = ", "), call. = FALSE)
    }
  }

  layer(
    stat = "identity",
    data = data,
    mapping = mapping,
    geom = GeomIrc,
    position = "identity",
    show.legend = NA,
    inherit.aes = TRUE,
    params = list(...)
  )
}

# Define the custom Geom for geom_irc
GeomIrc <- ggproto('GeomIrc', Geom,
                   required_aes = c("y", "ymin", "ymax", "color"),
                   draw_group = function(data, panel_params, coord, ...) {
                     grobTree(
                       geom_linerange(data = data, color = "black", aes(ymin = data$conf_low_irc, ymax = data$conf_high_irc), ...),
                       geom_linerange(data = data, aes(ymin = data$estimate_irc - 0.05, ymax = data$estimate_irc + 0.05), ...),
                       geom_linerange(data = data, aes(ymin = data$estimate_irc - 0.05, ymax = data$estimate_irc - 0.05 + data$estimate_vote * 0.1), ...),
                       geom_linerange(data = data, aes(ymin = data$estimate_irc - 0.05 + data$conf_low_vote * 0.1, ymax = data$estimate_irc - 0.05 + data$conf_high_vote * 0.1), ...)
                     )
                   }
)
