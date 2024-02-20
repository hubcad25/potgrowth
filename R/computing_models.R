#' Get RCI Model Data Based on Party and Model Selection
#'
#' This function filters and transforms data based on the specified model, party,
#' and other criteria. It supports two models: "vote_solidity" and "potential_for_growth".
#' It pivots data, filters by the highest or non-highest RCI values (based on the model),
#' filters by party, and selects specified columns.
#'
#' @param party The party to filter the data by.
#' @param model A character string specifying the model to use.
#'   Supported models are "vote_solidity" and "potential_for_growth".
#' @param data A dataframe containing the data to be processed.
#' @param rci_prefix A character string specifying the prefix for RCI columns.
#' @param attitudegap_prefix A character string specifying the prefix for attitude gap columns.
#' @param saliency_prefix A character string specifying the prefix for saliency columns.
#' @param ses_and_vis_to_include Variables to include in the final dataset, passed as unquoted names. Vector used in dplyr::select() function. Can use selection helpers from tidyselect such as starts_with().
#'
#' @return A dataframe filtered and transformed based on the specified parameters.
#' @export
#'
#' @examples
#' # Example usage:
#' # get_model_data(party = "CAQ",
#' #                model = "vote_solidity",
#' #                data = your_dataframe,
#' #                ses_and_vis_to_include = c(your_variables, starts_with("random_prefix")))
get_model_data <- function(
    party,
    model = c("vote_solidity", "potential_for_growth"),
    data,
    rci_prefix = "irc_",
    attitudegap_prefix = "attitudegap",
    saliency_prefix = "adj_saliency",
    ses_and_vis_to_include){

  # Verify model argument is valid
  if (!model %in% c("vote_solidity", "potential_for_growth")) {
    stop("Invalid model selection. Choose either 'vote_solidity' or 'potential_for_growth'.", call. = FALSE)
  }

  model <- match.arg(model) # Ensure 'model' matches one of the options

  ses_and_vis_to_include <- rlang::enquos(ses_and_vis_to_include)

  if (model == "vote_solidity"){
    newdata <- data %>%
      tidyr::pivot_longer(., cols = starts_with(rci_prefix),
                          names_to = "party",
                          names_prefix = rci_prefix,
                          values_to = "rci") %>%
      group_by(id, source_id) %>%
      filter(rci == max(rci))
  } else if (model == "potential_for_growth") {
    newdata <- data %>%
      tidyr::pivot_longer(., cols = starts_with(rci_prefix),
                          names_to = "party",
                          names_prefix = rci_prefix,
                          values_to = "rci") %>%
      group_by(id, source_id) %>%
      filter(rci != max(rci) | rci == 0)
  }

  subset_data <- newdata %>%
    ungroup() %>%
    ## filter for party
    filter(party == !!party) %>%
    dplyr::select(rci, starts_with(paste0(attitudegap_prefix, "_", party)),
                  starts_with(saliency_prefix), !!!ses_and_vis_to_include)

  names(subset_data) <- gsub(paste0("_", party), "", names(subset_data))
  return(subset_data)
}



