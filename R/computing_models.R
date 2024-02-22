#' Get RCI Model Data Based on Party and Model Selection
#'
#' This function filters and transforms data based on the specified model, party,
#' and other criteria. It supports two models: "vote_solidity" and "potential_for_growth".
#' It pivots data, filters by the highest or non-highest RCI values (based on the model),
#' filters by party, and selects specified columns.
#'
#' @param party The party to filter the data by.
#' @param model_type A character string specifying the model to use.
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
#' #                model_type = "vote_solidity",
#' #                data = your_dataframe,
#' #                ses_and_vis_to_include = c(your_variables, starts_with("random_prefix")))
get_model_data <- function(
    party,
    model_type = c("vote_solidity", "potential_for_growth"),
    data,
    rci_prefix = "irc_",
    attitudegap_prefix = "attitudegap",
    saliency_prefix = "adj_saliency",
    ses_and_vis_to_include){

  # Verify model argument is valid
  if (!model_type %in% c("vote_solidity", "potential_for_growth")) {
    stop("Invalid model selection. Choose either 'vote_solidity' or 'potential_for_growth'.", call. = FALSE)
  }

  model_type <- match.arg(model_type) # Ensure 'model' matches one of the options

  ses_and_vis_to_include <- rlang::enquos(ses_and_vis_to_include)

  if (model_type == "vote_solidity"){
    newdata <- data %>%
      tidyr::pivot_longer(., cols = starts_with(rci_prefix),
                          names_to = "party",
                          names_prefix = rci_prefix,
                          values_to = "rci") %>%
      group_by(id, source_id) %>%
      filter(rci == max(rci))
  } else if (model_type == "potential_for_growth") {
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

#' Get Data for Vote Intention Model
#'
#' This function filters and selects columns from a dataset based on criteria relevant for
#' analyzing vote intention. It selects the vote intention variable, attitude gap variables,
#' saliency variables, and any additional socio-economic status (SES) and visibility (vis)
#' variables specified by the user.
#'
#' @param data A dataframe containing the data to be processed.
#' @param voteint_variable The name of the vote intention variable in the dataset.
#' @param attitudegap_prefix A character string specifying the prefix for attitude gap columns.
#' @param saliency_prefix A character string specifying the prefix for saliency columns.
#' @param ses_and_vis_to_include Variables to include in the final dataset, passed as unquoted names. These should
#'   be socio-economic status (SES) and visibility (vis) variables that the user wishes to retain for analysis.
#'
#' @return A dataframe containing the selected columns based on the specified criteria.
#' @export
#'
#' @examples
#' # Assuming 'your_data' is your dataset and it contains variables for vote intention,
#' # attitude gaps, saliency, and specific SES and visibility variables you are interested in:
#' get_voteint_model_data(data = your_data,
#'                        voteint_variable = "voteInt",
#'                        ses_and_vis_to_include = c("age", "education"))
get_voteint_model_data <- function(
    data,
    voteint_variable = "voteInt",
    attitudegap_prefix = "attitudegap",
    saliency_prefix = "adj_saliency",
    ses_and_vis_to_include
){
  ses_and_vis_to_include <- rlang::enquos(ses_and_vis_to_include)
  subset_data <- data %>%
    dplyr::select(all_of(voteint_variable), starts_with(attitudegap_prefix),
                  starts_with(saliency_prefix),
                  !!!ses_and_vis_to_include)
  return(subset_data)
}

#' Linear Model with Interaction Terms Between Atittude Gaps and Issue Saliency
#'
#' Constructs and fits a linear model using a specified dependent variable and
#' interaction terms between attitude gap variables and saliency variables.
#' It automatically identifies variables starting with specified prefixes for
#' attitude gaps and saliency, creates interaction terms between each pair of
#' corresponding attitude and saliency variables, and includes these interactions
#' in the linear model along with other variables included in data.
#'
#' @param data A dataframe containing the dataset for analysis.
#' @param vd The name of the dependent variable in the model.
#' @param attitudegap_prefix A character string specifying the prefix for attitude gap columns.
#' @param saliency_prefix A character string specifying the prefix for saliency columns.
#'
#' @return A linear model object containing the fitted model with interaction terms.
#' @export
#'
#' @examples
#' # Assuming 'your_data' is a dataframe that contains a dependent variable 'rci',
#' # and multiple attitude gap and saliency variables with respective prefixes:
#' model <- votemodel_lm(data = your_data,
#'                               vd = "rci",
#'                               attitudegap_prefix = "attitudegap",
#'                               saliency_prefix = "adj_saliency")
#' summary(model)
votemodel_lm <- function(
    data,
    vd = "rci",
    attitudegap_prefix = "attitudegap",
    saliency_prefix = "adj_saliency"
)
{
  ## find variables that need interaction
  ### variables that start with attitudegap_prefix and saliency_prefix
  #### then we need to associate each variable to its variable
  ### 1. attitudegap_columns
  attitudegap_columns <- names(data %>% select(starts_with(attitudegap_prefix)))
  ### 2. attitudegap_scales: attitudegap_columns when removing the prefix and _ that follows
  attitudegap_scales <- gsub(paste0(attitudegap_prefix, "_"), "", attitudegap_columns)
  ### 3. saliency_columns
  saliency_columns <- names(data %>% select(starts_with(saliency_prefix)))
  ### 4. saliency_scales: saliency_columns when removing the prefix and _ that follows
  saliency_scales <- gsub(paste0(saliency_prefix, "_"), "", saliency_columns)
  ## 5. check to make sure each attitudegap_scales is in saliency_scales and vice versa
  if (sum(!(attitudegap_scales %in% saliency_scales)) > 0){
    stop("Some scales containing attitude gaps do not have saliency values.")
  }
  if (sum(!(saliency_scales %in% attitudegap_scales)) > 0){
    stop("Some scales containing saliency values do not have attitude gaps.")
  }
  ### 6. Create interaction term with all variables by associating each variable
  #####  from attitudegap_columns with its equivalent in saliency_columns
  interactions_vector <- sapply(
    X = attitudegap_scales,
    FUN = function(x) {
      paste0(attitudegap_prefix, "_", x, " * ", saliency_prefix, "_", x)
    }
  )
  interactions_terms <- paste0(interactions_vector, collapse = " + ")
  formula <- as.formula(paste0(vd, " ~ . + ", interactions_terms))
  model <- lm(formula = formula,
              data = data)
  return(model)
}


#' Multinomial Logistic Regression with Interaction Terms
#'
#' This function prepares and fits a multinomial logistic regression model using the `nnet` package,
#' with a specified vote intention variable as the dependent variable. It automatically creates interaction terms
#' between attitude gap variables and saliency variables for specified parties, incorporating these interactions
#' into the model.
#'
#' @param data A dataframe containing the dataset for analysis.
#' @param parties A character vector of party names to be used in creating interaction terms.
#' @param vd The name of the vote intention variable in the dataset.
#' @param attitudegap_prefix A character string specifying the prefix for attitude gap columns.
#' @param saliency_prefix A character string specifying the prefix for saliency columns.
#'
#' @return A multinomial logistic regression model object from the `nnet` package.
#' @export
#'
#' @examples
#' # Assuming 'your_data' is a dataframe that contains a vote intention variable,
#' # attitude gap variables, and saliency variables for different parties:
#' parties <- c("Party1", "Party2")
#' model <- votemodel_multinom(data = your_data,
#'                                     parties = parties,
#'                                     vd = "voteInt",
#'                                     attitudegap_prefix = "attitudegap",
#'                                     saliency_prefix = "adj_saliency")
#' summary(model)
#'
#' @importFrom nnet multinom
#' @import dplyr
votemodel_multinom <- function(
    data,
    parties,
    vd = "voteInt",
    attitudegap_prefix = "attitudegap",
    saliency_prefix = "adj_saliency"
){
  ## find variables that need interaction
  ### variables that start with attitudegap_prefix and saliency_prefix
  #### then we need to associate each variable to its variable
  ### 1. attitudegap_columns
  attitudegap_columns <- names(data %>% dplyr::select(starts_with(attitudegap_prefix)))
  ### 2. attitudegap_scales: attitudegap_columns when removing the prefix and _ that follows
  attitudegap_scales <- gsub(paste0(attitudegap_prefix, "_"), "", attitudegap_columns)
  for (i in parties){
    attitudegap_scales <- gsub(paste0(i, "_"), "", attitudegap_scales)
  }
  attitudegap_scales <- unique(attitudegap_scales)
  ### 3. saliency_columns
  saliency_columns <- names(data %>% dplyr::select(starts_with(saliency_prefix)))
  ### 4. saliency_scales: saliency_columns when removing the prefix and _ that follows
  saliency_scales <- gsub(paste0(saliency_prefix, "_"), "", saliency_columns)
  ## 5. check to make sure each attitudegap_scales is in saliency_scales and vice versa
  if (sum(!(attitudegap_scales %in% saliency_scales)) > 0){
    stop("Some scales containing attitude gaps do not have saliency values.")
  }
  if (sum(!(saliency_scales %in% attitudegap_scales)) > 0){
    stop("Some scales containing saliency values do not have attitude gaps.")
  }
  ### 6. Create interaction term with all variables by associating each variable
  #####  from attitudegap_columns with its equivalent in saliency_columns
  interactions_vector <- unlist(lapply(
    X = parties,
    FUN = function(party) {
      party_vector <- sapply(
        X = attitudegap_scales,
        FUN = function(scale){
          paste0(attitudegap_prefix, "_", party, "_", scale, " * ", saliency_prefix, "_", scale)
        }
      )
    }
  ))
  interactions_terms <- paste0(interactions_vector, collapse = " + ")
  formula <- as.formula(paste0(vd, " ~ . + ", interactions_terms))
  model <- nnet::multinom(formula = formula,
                          data = data)
  return(model)
}


#' Independant Variable (IV) Linear Model
#'
#' Fits a linear model to predict a specified independant variable (IV) based on
#' one or more socio-economic status (SES) variables provided by the user. This function
#' is designed to facilitate the creation of linear models by abstracting away the formula
#' creation process and directly using column names provided as strings.
#'
#' @param data A dataframe containing the dataset for the analysis.
#' @param iv_to_predict A string specifying the name of the independant variable that
#'   the model will predict. This variable should be present in `data`.
#' @param ses A character vector of names of socio-economic status (SES) variables
#'   included as predictors in the model. These variables should be present in `data`.
#'
#' @return An object of class `lm` representing the fitted linear model.
#' @export
#'
#' @examples
#' # Assuming 'df' is your dataframe, 'attitude_on_immigration' is the independant variable, and
#' # 'age' and 'education' are SES variables:
#' model <- iv_model(data = df, iv_to_predict = "attitude_on_immigration", ses = c("age", "education"))
#' summary(model)
iv_model <- function(
    data,
    iv_to_predict,
    ses
){
  model_data <- data %>%
    select(all_of(c(iv_to_predict, ses)))
  formula <- as.formula(paste0(iv_to_predict, " ~ ."))
  model <- lm(formula = formula,
              data = model_data)
  return(model)
}
