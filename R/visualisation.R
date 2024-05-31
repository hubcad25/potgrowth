#' Create a Composite Geom for IRC and Vote Estimates
#'
#' This function creates a list of `geom_linerange` elements to represent the
#' confidence intervals and estimates for IRC and vote values.
#'
#' @param conf_low_irc Lower bound of the IRC confidence interval.
#' @param conf_high_irc Upper bound of the IRC confidence interval.
#' @param estimate_irc Point estimate for IRC.
#' @param conf_low_vote Lower bound of the vote confidence interval.
#' @param conf_high_vote Upper bound of the vote confidence interval.
#' @param estimate_vote Point estimate for the vote.
#' @param irc_conf_int_color Color for the IRC confidence interval. Default is "black".
#' @param ... Additional parameters passed to `geom_linerange`.
#' @param base_size Base size for text and line elements. Default is 3.5.
#' @param dodge Amount to dodge the elements. Default is 0.
#'
#' @return A list of `geom_linerange` objects for use with ggplot2.
#' @import ggplot2
#' @examples
#' ggplot(data, aes(x = your_x_variable, y = your_y_variable)) +
#'   geom_irc(conf_low_irc = your_data$conf_low_irc,
#'            conf_high_irc = your_data$conf_high_irc,
#'            estimate_irc = your_data$estimate_irc,
#'            conf_low_vote = your_data$conf_low_vote,
#'            conf_high_vote = your_data$conf_high_vote,
#'            estimate_vote = your_data$estimate_vote,
#'            aes(color = your_color_variable))
geom_irc <- function(conf_low_irc, conf_high_irc, estimate_irc,
                     conf_low_vote, conf_high_vote, estimate_vote,
                     irc_conf_int_color = "black", ...,
                     base_size = 3.5, dodge = 0) {
  list(
    geom_linerange(aes(ymin = conf_low_irc, ymax = conf_high_irc),
                   color = irc_conf_int_color, position = position_dodge(width = dodge),
                   size = 0.29 * base_size, ...),
    geom_linerange(aes(ymin = estimate_irc - 0.05, ymax = estimate_irc + 0.05),
                   position = position_dodge(width = dodge),
                   size = base_size, color = "grey80", ...),
    geom_linerange(aes(ymin = estimate_irc - 0.05,
                       ymax = estimate_irc - 0.05 + estimate_vote * 0.1),
                   position = position_dodge(width = dodge),
                   size = base_size, alpha = 0.3, ...),
    geom_linerange(aes(ymin = estimate_irc - 0.05 + conf_low_vote * 0.1,
                       ymax = estimate_irc - 0.05 + conf_high_vote * 0.1),
                   position = position_dodge(width = dodge),
                   size = 0.5 * base_size, alpha = 0.1, ...),
    geom_text(aes(y = estimate_irc - 0.045 + estimate_vote * 0.1,
                  label = paste0(round(estimate_vote * 100), "%")),
              position = position_dodge(width = dodge),
              angle = 90, vjust = -1.2, size = 0.57 * base_size, color = "black")
  )
}
