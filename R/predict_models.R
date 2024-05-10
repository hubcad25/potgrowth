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


#' Predict Dynamic Potential for Growth Data with SES Interactions
#'
#' This function processes a dataset to model and predict potential growth
#' data for various political parties and issues, including optional SES interactions.
#' It uses both linear and logistic regression to predict IRC and voter likelihoods respectively.
#' The function processes each party and issue combination, generating a set
#' of predictions with confidence intervals. Supports interaction with up to two
#' SES variables.
#'
#' @param data A dataframe containing the original dataset to be processed.
#' @param parties A vector of political party identifiers to be analyzed.
#' @param issues A vector of issue identifiers to be analyzed.
#' @param ses_interactions A character vector of up to two SES variable names to be used for interaction
#'        with the 'position' variable in the models. Defaults to an empty character vector.
#' @param model_variables A character vector of additional model variables to include
#'        in the regression models. Defaults to c("region", "age_cat", "income",
#'        "male", "educ", "lang", "religion").
#'
#' @return A dataframe combining all the predictions across the specified parties
#'         and issues. Each row contains the estimates and confidence intervals
#'         for IRC and voter likelihood, along with party and issue information.
#'
#' @importFrom dplyr select filter mutate rename
#' @importFrom stats lm glm
#' @importFrom marginaleffects predictions datagrid
#'
#' @examples
#' test <- dynamic_potgrowth_data(
#'   data = data,
#'   parties = c("CAQ", "QS"),
#'   issues = issues,
#'   ses_interactions = c("region"),
#' )
#'
#' @export
dynamic_potgrowth_data <- function(
    data,
    parties,
    issues,
    ses_interactions = character(0),
    model_variables = c("region", "age_cat", "income", "male", "educ", "lang", "religion")
){
  # Stop the function if more than two SES variables are provided
  if (length(ses_interactions) > 2) {
    stop("The function only supports up to two interactions with SES variables.")
  }
  model_variables <- model_variables[!(model_variables %in% ses_interactions)]
  data_model <- data %>%
    select(irc, voter, party, issue, position, all_of(ses_interactions), all_of(model_variables))
  ## for each party
  for (i in 1:length(parties)){
    partyi <- parties[i]
    data_model_party <- data_model %>%
      filter(party == partyi)
    ## for each issue
    for (j in 1:length(issues)){
      issue_j <- issues[j]
      data_model_j <- data_model_party %>%
        filter(issue == issue_j) %>%
        mutate(position = factor(position, ordered = FALSE))
      data_model_j$position <- relevel(data_model_j$position,
                                       ref = potgrowth::mode(data_model_j$position))
      ## if no ses_interactions
      if (length(ses_interactions) == 0){
        model_irc <- data_model_j %>%
          filter(voter == 0) %>%
          select(irc, position, all_of(model_variables)) %>%
          lm(irc ~ .,
             data = .)
        preds_irc <- marginaleffects::predictions(
          model_irc,
          newdata = marginaleffects::datagrid(
            model = model_irc,
            position = levels(data_model_j$position)
          ),
          conf_level = 0.99
        ) %>%
          select(
            position,
            estimate_irc = estimate,
            conf_low_irc = conf.low,
            conf_high_irc = conf.high
          )
        model_vote <- data_model_j %>%
          select(voter, position, all_of(model_variables)) %>%
          glm(voter ~ .,
              data = .,
              family = binomial(link = "logit"))
        preds_vote <- marginaleffects::predictions(
          model_vote,
          newdata = marginaleffects::datagrid(
            model = model_vote,
            position = levels(data_model_j$position)
          ),
          type = "response",
          conf_level = 0.99
        ) %>%
          select(
            position,
            estimate_vote = estimate,
            conf_low_vote = conf.low,
            conf_high_vote = conf.high
          )
        df_preds_j <- left_join(preds_irc, preds_vote, by = "position") %>%
          mutate(
            issue = issue_j,
            position = as.character(position)
          ) %>%
          relocate(issue)
        if (j == 1){
          df_preds_i <- df_preds_j
        } else {
          df_preds_i <- rbind(df_preds_i, df_preds_j)
        }
        ### only 1 ses_interactions
      } else if (length(ses_interactions) == 1){
        ## irc model
        irc_formula <- as.formula("irc ~ . + position * ses")
        model_irc <- data_model_j %>%
          filter(voter == 0) %>%
          rename(ses = ses_interactions[1]) %>%
          select(irc, position, ses, all_of(model_variables)) %>%
          lm(formula = irc_formula,
             data = .)
        preds_irc <- marginaleffects::predictions(
          model_irc,
          newdata = marginaleffects::datagrid(
            model = model_irc,
            position = levels(data_model_j$position),
            ses = unique(model_irc$model$ses)
          ),
          conf_level = 0.99
        ) %>%
          select(
            position,
            ses,
            estimate_irc = estimate,
            conf_low_irc = conf.low,
            conf_high_irc = conf.high
          )
        ## vote model
        vote_formula <- as.formula("voter ~ . + position * ses")
        model_vote <- data_model_j %>%
          rename(ses = ses_interactions[1]) %>%
          select(voter, position, ses, all_of(model_variables)) %>%
          glm(formula = vote_formula,
              data = .,
              family = binomial(link = "logit"))
        preds_vote <- marginaleffects::predictions(
          model_vote,
          newdata = marginaleffects::datagrid(
            model = model_vote,
            position = levels(data_model_j$position),
            ses = unique(model_vote$model$ses)
          ),
          type = "response",
          conf_level = 0.99
        ) %>%
          select(
            position,
            ses,
            estimate_vote = estimate,
            conf_low_vote = conf.low,
            conf_high_vote = conf.high
          )
        df_preds_j <- left_join(preds_irc, preds_vote, by = c("position", "ses")) %>%
          mutate(
            issue = issue_j,
            position = as.character(position)
          ) %>%
          rename(!!ses_interactions[1] := ses) %>%
          relocate(issue)
        if (j == 1){
          df_preds_i <- df_preds_j
        } else {
          df_preds_i <- rbind(df_preds_i, df_preds_j)
        }
        ## if 2 ses_interactions
      } else if (length(ses_interactions == 2)){
        ## irc model
        irc_formula <- as.formula("irc ~ . + position * ses1 * ses2")
        model_irc <- data_model_j %>%
          filter(voter == 0) %>%
          rename(ses1 = ses_interactions[1],
                 ses2 = ses_interactions[2]) %>%
          select(irc, position, ses1, ses2, all_of(model_variables)) %>%
          lm(formula = irc_formula,
             data = .)
        preds_irc <- marginaleffects::predictions(
          model_irc,
          newdata = marginaleffects::datagrid(
            model = model_irc,
            position = levels(data_model_j$position),
            ses1 = unique(model_irc$model$ses1),
            ses2 = unique(model_irc$model$ses2)
          ),
          conf_level = 0.99
        ) %>%
          select(
            position,
            ses1,
            ses2,
            estimate_irc = estimate,
            conf_low_irc = conf.low,
            conf_high_irc = conf.high
          )
        ## vote model
        vote_formula <- as.formula("voter ~ . + position * ses1 * ses2")
        model_vote <- data_model_j %>%
          rename(ses1 = ses_interactions[1],
                 ses2 = ses_interactions[2]) %>%
          select(voter, position, ses1, ses2, all_of(model_variables)) %>%
          glm(formula = vote_formula,
              data = .,
              family = binomial(link = "logit"))
        preds_vote <- marginaleffects::predictions(
          model_vote,
          newdata = marginaleffects::datagrid(
            model = model_vote,
            position = levels(data_model_j$position),
            ses1 = unique(model_vote$model$ses1),
            ses2 = unique(model_vote$model$ses2)
          ),
          type = "response",
          conf_level = 0.99
        ) %>%
          select(
            position,
            ses1,
            ses2,
            estimate_vote = estimate,
            conf_low_vote = conf.low,
            conf_high_vote = conf.high
          )
        df_preds_j <- left_join(preds_irc, preds_vote, by = c("position", "ses1", "ses2")) %>%
          mutate(
            issue = issue_j,
            position = as.character(position)
          ) %>%
          rename(!!ses_interactions[1] := ses1,
                 !!ses_interactions[2] := ses2) %>%
          relocate(issue)
        if (j == 1){
          df_preds_i <- df_preds_j
        } else {
          df_preds_i <- rbind(df_preds_i, df_preds_j)
        }
      }
    }
    ## end of loop j
    df_preds_i <- df_preds_i %>%
      mutate(party = partyi) %>%
      relocate(party)
    if (i == 1){
      df_preds <- df_preds_i
    } else {
      df_preds <- rbind(df_preds, df_preds_i)
    }
    message(partyi)
  }
  return(df_preds)
}
