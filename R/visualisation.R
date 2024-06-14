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

#' Generate a Text-Based Progress Bar
#'
#' This function generates a text-based progress bar using Unicode characters.
#' The progress bar visually represents the percentage completion as a series
#' of filled and empty blocks.
#'
#' @param percentage A numeric value between 0 and 1 representing the percentage
#'   completion. For example, 0.75 represents 75% completion.
#'
#' @return A character string representing the progress bar. The progress bar
#'   consists of filled blocks (`█`), empty blocks (`░`), and a percentage value.
#'   The returned string is suitable for use in text-based outputs, such as
#'   console messages or tooltips in graphical displays.
#'
#' @examples
#' # Example usage:
#' generate_progress_bar(0.75)
#' # [1] "█████████░ 75%"
#'
#' generate_progress_bar(0.3)
#' # [1] "███░░░░░░░ 30%"
#'
#' @export
generate_progress_bar <- function(percentage) {
  filled_blocks = round(percentage * 10)
  empty_blocks = 10 - filled_blocks
  paste0(
    strrep("█", filled_blocks),
    strrep("░", empty_blocks),
    " ", round(percentage * 100), "%"
  )
}



#' Generate a Quarto Graph
#'
#' This function generates a Plotly graph for a given issue and survey data, with specific handling for the "iss_nationalisme_souv" issue.
#'
#' @param survey_data A data frame containing the survey data. It should include columns for `issue` and `position`.
#' @param issue_slug A string indicating the issue identifier.
#' @param choices A named vector of choices for the positions.
#' @param xlabel A string for the x-axis label.
#'
#' @return A Plotly graph object.
#' @import dplyr
#' @import plotly
#' @export
#'
#' @examples
#' survey_data <- data.frame(issue = c("iss_nationalisme_souv", "iss_other"),
#'                           position = c(0.25, 0.75),
#'                           other_columns = c(1, 2))
#' choices <- c("0.25" = "Low", "0.75" = "High")
#' xlabel <- "Position"
#' get_quarto_graph(survey_data, "iss_nationalisme_souv", choices, xlabel)
get_quarto_graph <- function(survey_data,
                             issue_slug,
                             party_positions,
                             choices,
                             xlabel,
                             remove_variables_from_models = NULL
                             ) {
  model_variables <- eval(formals(potgrowth::dynamic_potgrowth_data)$model_variables)
  model_variables <- model_variables[!model_variables %in% remove_variables_from_models]
  graph_data <- potgrowth::dynamic_potgrowth_data(
    data = survey_data,
    parties = potgrowth::qc_parties,
    issues = issue_slug,
    model_variables = model_variables
  ) %>%
    dplyr::mutate(
      estimate_irc = ifelse(estimate_irc > 0, 0, estimate_irc),
      estimate_irc = ifelse(estimate_irc < -1, -1, estimate_irc),
      conf_low_irc = ifelse(conf_low_irc > 0, 0, conf_low_irc),
      conf_low_irc = ifelse(conf_low_irc < -1, -1, conf_low_irc))
  if (issue_slug == "iss_nationalisme_souv") {
    graph_data <- graph_data %>%
      dplyr::mutate(
        position = ifelse(position == 0.25, 0.33, position),
        position = ifelse(position == 0.75, 0.67, position)
      ) %>%
      dplyr::filter(position != "0.5") %>%
      dplyr::left_join(party_positions, by = c("party", "issue")) %>%
      dplyr::mutate(is_party_position = ifelse(position == party_position, 1, 0))
  } else {
    graph_data <- graph_data %>%
      dplyr::left_join(party_positions, by = c("party", "issue")) %>%
      dplyr::mutate(is_party_position = ifelse(position == party_position, 1, 0))
  }
  graph_data2 <- as.data.frame(graph_data) %>%
    dplyr::mutate(sd = (conf_high_irc - conf_low_irc) / 2,
           progress_bar = sapply(estimate_vote, potgrowth::generate_progress_bar),
           line_opacity = ifelse(is_party_position == 1, 0.3, 0),
           xticklabel = choices[position])
  party_positions <- graph_data2 %>%
    dplyr::filter(is_party_position == 1)
  # Créer le graphique Plotly avec des barres en arrière-plan et un axe y secondaire
  p <- plot_ly(
    colors = potgrowth::qc_party_colors,
    width = 690) %>%
    add_markers(data = party_positions,
                text = ~paste0(party, "'s position:\n", xticklabel),
                hoverinfo = "text",
                x = ~position,
                y = ~estimate_irc,
                split = ~party,
                color = ~party,
                legendgroup = ~party,
                colors = potgrowth::qc_party_colors,
                marker = list(size = 40,
                              symbol = "diamond",
                              opacity = ~line_opacity),
                showlegend = FALSE) %>%
    add_lines(data = graph_data2,
              line = list(width = 1),
              showlegend = FALSE,
              x = ~position,
              y = ~estimate_irc,
              split = ~party,
              color = ~party,
              legendgroup = ~party,
              colors = potgrowth::qc_party_colors,
              hoverinfo = "none") %>%
    add_markers(x = ~position,
                y = ~estimate_irc,
                split = ~party,
                color = ~party,
                legendgroup = ~party,
                marker = list(size = 11),
                error_y = list(array = ~ sd),
                text = ~paste0("Acquired votes in segment<br>", progress_bar),
                hoverinfo = 'text') %>%
    layout(
      yaxis = list(range = c(-1, 0),
                   title = list(text = "Potential for Growth\n(predicted RCI of non-voters)",
                                standoff = 15),
                   tickvals = seq(from = -1, to = 0, by = 0.1),
                   ticktext = paste0(seq(from = -10, to = 0, by = 1), "   "),
                   zeroline = FALSE),
      xaxis = list(title = list(text = paste0("\n", xlabel, "\n")),
                   tickvals = names(choices),
                   ticktext = choices,
                   tickfont = list(size = 9.5),
                   ticklabelposition = "outside",
                   zeroline = FALSE),
      #annotations = list(text = "Diamonds indicate the parties' positions on the issue. Data from 2022.",
      #                   font = list(size = 10),
      #                   #standoff = 30,
      #                   showarrow = FALSE,
      #                   yanchor='auto',
      #                   xref = 'paper', x = 0,
      #                   yref = 'paper', y = -0.65),
      autosize = FALSE,
      margin = list(l = 25, r = 25, b = 25, t = 25)
    )
  return(p)
}


#' Generate Quarto Party Dashboard
#'
#' This function generates a party dashboard using `plotly` based on survey data, issues data frame, and party positions.
#'
#' @param party A character vector indicating the party.
#' @param survey_data A data frame containing the survey data.
#' @param issues_df A data frame containing issue slugs, labels, and position labels.
#' @param party_positions A data frame containing party positions on issues.
#' @param remove_variables_from_models A character vector of variables to be removed from the model.
#'
#' @return A `plotly` object representing the party dashboard.
#' @import dplyr
#' @import plotly
#' @export
#'
#' @examples
#' \dontrun{
#' party <- "PartyName"
#' survey_data <- data.frame(...) # Provide your survey data here
#' issues_df <- data.frame(issue_slug = c("issue1", "issue2"), issue_label = c("Issue 1", "Issue 2"), label0 = c("Less", "Less"), label1 = c("More", "More"))
#' party_positions <- data.frame(...) # Provide your party positions data here
#' remove_variables_from_models <- c("variable1", "variable2")
#' p <- get_quarto_party_dashboard(party, survey_data, issues_df, party_positions, remove_variables_from_models)
#' p
#' }
get_quarto_party_dashboard <- function(party, survey_data, issues_df, party_positions, remove_variables_from_models, width = 700) {
  model_variables <- eval(formals(potgrowth::dynamic_potgrowth_data)$model_variables)
  model_variables <- model_variables[!model_variables %in% remove_variables_from_models]
  graph_data <- potgrowth::dynamic_potgrowth_data(
    data = survey_data,
    parties = party,
    issues = issues_df$issue_slug,
    model_variables = model_variables
  ) %>%
    dplyr::mutate(
      estimate_irc = ifelse(estimate_irc > 0, 0, estimate_irc),
      estimate_irc = ifelse(estimate_irc < -1, -1, estimate_irc),
      conf_low_irc = ifelse(conf_low_irc > 0, 0, conf_low_irc),
      conf_low_irc = ifelse(conf_low_irc < -1, -1, conf_low_irc),
      position = ifelse(position == 0.25, 0.33, position),
      position = ifelse(position == 0.75, 0.67, position)) %>%
    dplyr::filter(position != 0.5) %>%
    dplyr::left_join(party_positions, by = c("party", "issue")) %>%
    dplyr::mutate(is_party_position = ifelse(position == party_position, 1, 0))
  issue_labels_vec <- setNames(issues_df$issue_label, issues_df$issue_slug)
  labels0 <- setNames(issues_df$label0, issues_df$issue_slug)
  labels1 <- setNames(issues_df$label1, issues_df$issue_slug)
  graph_data2 <- as.data.frame(graph_data) %>%
    dplyr::mutate(sd = (conf_high_irc - conf_low_irc) / 2,
           progress_bar = sapply(estimate_vote, potgrowth::generate_progress_bar),
           line_opacity = ifelse(is_party_position == 1, 0.3, 0),
           issue_label = issue_labels_vec[issue],
           position_label = ifelse(position == 0, labels0[issue], ""),
           position_label = ifelse(position == 1, labels1[issue], position_label)
    )
  party_positions <- graph_data2 %>%
    dplyr::filter(is_party_position == 1)
  p <- plot_ly(
    colors = issue_colors,
    width = width) %>%
    add_markers(data = party_positions,
                hoverinfo = "none",
                x = ~position,
                y = ~estimate_irc,
                split = ~issue,
                color = ~issue,
                legendgroup = ~issue,
                colors = issue_colors,
                marker = list(size = 40,
                              symbol = "diamond",
                              opacity = ~line_opacity),
                showlegend = FALSE) %>%
    add_lines(data = graph_data2,
              line = list(width = 1),
              showlegend = FALSE,
              x = ~position,
              y = ~estimate_irc,
              split = ~issue,
              color = ~issue,
              legendgroup = ~issue,
              colors = issue_colors,
              hoverinfo = "none") %>%
    add_markers(x = ~position,
                y = ~estimate_irc,
                split = ~issue,
                color = ~issue,
                legendgroup = ~issue,
                name = ~issue_label,
                marker = list(size = 11),
                error_y = list(array = ~ sd),
                text = ~paste0(issue_label, ", ", position_label, "\nAcquired votes in segment<br>", progress_bar),
                hoverinfo = 'text') %>%
    layout(
      legend = list(
        font = list(size = 8),
        x = 4,
        xanchor = 'right',
        traceorder = 'normal',
        itemsizing = 'constant',
        ncol = 2,
        title = list(text = ''), # Enlever le titre de la légende
        tracegroupgap = 20 # Espace entre les groupes
      ),
      yaxis = list(range = c(-1, 0),
                   title = list(text = "Potential for Growth\n(predicted RCI of non-voters)",
                                standoff = 15),
                   tickvals = seq(from = -1, to = 0, by = 0.1),
                   ticktext = paste0(seq(from = -10, to = 0, by = 1), "   "),
                   zeroline = FALSE),
      xaxis = list(title = list(text = "Position on the issue"),
                   tickvals = c(0, 0.33, 0.67, 1),
                   ticktext = c("", "", "", ""),
                   zeroline = FALSE),
      autosize = FALSE,
      margin = list(l = 25, r = 25, b = 25, t = 25)
    )
  return(p)
}


