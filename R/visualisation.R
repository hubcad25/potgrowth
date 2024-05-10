#' Create Custom Geom Layers for IRC Visualization
#'
#' This function returns layers of `geom_linerange` designed specifically for visualizing
#' the IRC along with its confidence intervals and other
#' related estimates. The function is intended to be added to an existing `ggplot` object.
#'
#' @param mapping Set of aesthetic mappings created by `aes()` or `aes_()`. Expected aesthetics
#'   are `conf_low_irc`, `conf_high_irc`, `estimate_irc`, `estimate_vote`, `conf_low_vote`, `conf_high_vote`.
#' @param color The default color for the lines, which can be overridden individually in each `geom_linerange`.
#' @param ... Additional parameters for fine-tuning the `geom_linerange` components, such as `size` and `alpha`.
#' @return A list of `geom_linerange` layers.
#' @examples
#' df <- data.frame(
#'   conf_low_irc = rnorm(10, 0.1, 0.05),
#'   conf_high_irc = rnorm(10, 0.2, 0.05),
#'   estimate_irc = rnorm(10, 0.15, 0.05),
#'   estimate_vote = runif(10, 0.4, 0.6),
#'   conf_low_vote = runif(10, 0.35, 0.45),
#'   conf_high_vote = runif(10, 0.55, 0.65)
#' )
#' ggplot(df, aes(conf_low_irc = conf_low_irc, conf_high_irc = conf_high_irc,
#'                estimate_irc = estimate_irc, estimate_vote = estimate_vote,
#'                conf_low_vote = conf_low_vote, conf_high_vote = conf_high_vote)) +
#'   geom_irc()
#' @export
geom_irc <- function(mapping = NULL, ...) {
  list(
    geom_linerange(mapping = mapping, aes(ymin = conf_low_irc, ymax = conf_high_irc), ...),
    geom_linerange(mapping = mapping, aes(ymin = estimate_irc - 0.05, ymax = estimate_irc + 0.05), ...),
    geom_linerange(mapping = mapping, aes(ymin = estimate_irc - 0.05,
                                          ymax = estimate_irc - 0.05 + estimate_vote * 0.1), ...),
    geom_linerange(mapping = mapping, aes(ymin = estimate_irc - 0.05 + conf_low_vote * 0.1,
                                          ymax = estimate_irc - 0.05 + conf_high_vote * 0.1), ...)
  )
}

#' Create Custom Geom Layers for IRC Visualization
#'
#' This function returns layers of `geom_linerange` designed specifically for visualizing
#' the IRC along with its confidence intervals and other
#' related estimates. The function is intended to be added to an existing `ggplot` object.
#'
#' @param mapping Set of aesthetic mappings created by `aes()` or `aes_()`. Expected aesthetics
#'   are `conf_low_irc`, `conf_high_irc`, `estimate_irc`, `estimate_vote`, `conf_low_vote`, `conf_high_vote`.
#' @param color The default color for the lines, which can be overridden individually in each `geom_linerange`.
#' @param ... Additional parameters for fine-tuning the `geom_linerange` components, such as `size` and `alpha`.
#' @return A list of `geom_linerange` layers.
#' @examples
#' df <- data.frame(
#'   conf_low_irc = rnorm(10, 0.1, 0.05),
#'   conf_high_irc = rnorm(10, 0.2, 0.05),
#'   estimate_irc = rnorm(10, 0.15, 0.05),
#'   estimate_vote = runif(10, 0.4, 0.6),
#'   conf_low_vote = runif(10, 0.35, 0.45),
#'   conf_high_vote = runif(10, 0.55, 0.65)
#' )
#' ggplot(df, aes(conf_low_irc = conf_low_irc, conf_high_irc = conf_high_irc,
#'                estimate_irc = estimate_irc, estimate_vote = estimate_vote,
#'                conf_low_vote = conf_low_vote, conf_high_vote = conf_high_vote)) +
#'   geom_irc()
#' @export
geom_irc2 <- function(mapping = NULL, ...) {
  list(
    geom_linerange(mapping = mapping, aes(ymin = conf_low_irc, ymax = conf_high_irc), ...)
  )
}

