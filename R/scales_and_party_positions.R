#' Compute Scale Score from Factor Analysis Loadings
#'
#' This function computes scale scores based on factor analysis loadings and survey data.
#' It takes a factor analysis object, survey data, and the order of the scale as inputs.
#' It returns the computed scale score for the specified scale.
#'
#' @param factanal_object An object of class 'factanal' representing the result of a factor analysis.
#' @param survey_data A data frame containing survey data where column names match the variables used in the factor analysis.
#' @param scale_order A numeric or character vector indicating the columns of the loadings to be used.
#' @return A numeric vector of scale scores for the survey data based on the specified factor analysis loadings.
#' @examples
#' # Assume factanal_object is the result of a factor analysis and df is your survey data
#' # compute_scale_score(factanal_object, df, 1) # For the first factor
#' @export
compute_scale_scores <- function(factanal_object, survey_data, scale_order) {
  # Create vector containing factor loadings
  loadings <- factanal_object$loadings[, scale_order]

  # Check if survey data contains all variables in loadings
  if (!all(names(loadings) %in% names(survey_data))) {
    missing_vars <- setdiff(names(loadings), names(survey_data))
    stop("Survey data is missing the following variables required for the scale: ", paste(missing_vars, collapse = ", "), ".")
  }

  # Convert survey_data and the wanted variables in matrix for matrix operations
  data_matrix <- as.matrix(survey_data[,names(loadings)])

  # Matrix operation to compute scale scores
  scale_scores <- data_matrix %*% loadings
  scale_scores <- scale_scores[, 1]

  ## Adjust scale scores if any are less than 0
  min_score <- min(scale_scores)
  if (min_score < 0) {
    scale_scores <- scale_scores - min_score # Augmenter pour que min soit 0
  }

  ## Normaliser les scores pour qu'ils soient entre 0 et 1
  max_score <- max(scale_scores)
  if (max_score > 0) { # Éviter la division par zéro
    scale_scores <- scale_scores / max_score
  }

  return(scale_scores)
}


#' Read a Single ODS File and Append Filename as a Column
#'
#' This function reads a single ODS file from the specified path,
#' appends the filename (without extension) as a new column to the data,
#' and returns the modified data frame. The filename is used to identify
#' the source of the data within the returned data frame.
#'
#' @param path The file path to the ODS file to be read.
#'
#' @return A data frame with the contents of the ODS file and an additional
#'         column named `coder` containing the filename from which the data was read.
#'
#' @examples
#' # Assuming you have an ODS file named "data_file.ods" in your working directory:
#' path <- system.file("extdata", "data_file.ods", package = "YourPackageName")
#' data <- read_one_file(path)
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom readODS read_ods
#' @export
read_one_file <- function(path) {
  filename <- tools::file_path_sans_ext(basename(path))
  data <- readODS::read_ods(path)
  data$coder <- filename
  return(data)
}


#' Aggregate Party Positions from Survey Data
#'
#' Reads multiple ODS files from a specified directory, excluding any templates, and aggregates the data
#' into a single dataframe. It then pivots the data from wide to long format based on specified column names
#' and calculates the mean position for each party by variable.
#'
#' @param path The directory path containing ODS files to read.
#' @param colnames Vector of column names to pivot from wide to long format, representing parties.
#'
#' @return A tibble data frame in long format with variables for each party and their mean positions.
#'
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom tidyr pivot_longer
#' @import readODS
#' @import tools
#' @export
#' @examples
#' # This example assumes you have a directory with ODS files for party position data
#' # Replace `path_to_directory` with the actual path to your ODS files
#' aggregate_party_positions("path_to_directory", c("CAQ", "PLQ", "QS", "PQ", "PCQ"))
aggregate_party_positions <- function(path, colnames) {
  files <- list.files(path, full.names = TRUE)
  # Remove template files
  files <- files[grep("template", files, invert = TRUE)]

  # Read files, append filename as a coder column, and combine
  wide <- dplyr::bind_rows(lapply(files, read_one_file))

  # Pivot data to long format and calculate mean position by party and variable
  df <- tidyr::pivot_longer(wide, cols = colnames, names_to = "party", values_to = "position") %>%
    dplyr::group_by(VARIABLE, party) %>%
    dplyr::summarise(position = mean(position, na.rm = TRUE), .groups = 'drop')

  return(df)
}


#' Compute Party Scales from Aggregated Issue Positions
#'
#' This function takes aggregated issue positions for different parties, a factor analysis object,
#' and a vector of scale names. It returns a matrix of scale scores for each party based on
#' the specified scales in the factor analysis object.
#'
#' @param aggregated_issue_positions A data frame of aggregated issue positions for parties.
#'        Expected to be the output from `potgrowth::aggregate_party_positions`.
#' @param factanal_object A factor analysis object containing loadings for scales.
#'        Expected to have loadings corresponding to the scales specified in `scale_names`.
#' @param scale_names A character vector specifying the names of the scales in the order
#'        they appear in the factor analysis object loadings.
#'
#' @return A matrix with parties as row names and scales as column names, containing
#'         scale scores for each party.
#'
#' @importFrom dplyr mutate arrange
#' @importFrom tidyr pivot_wider
#' @importFrom potgrowth compute_scale_scores
#' @export
#' @examples
#' # Assuming `aggregated_issue_positions` and `factanal_object` are defined:
#' scale_names <- c("souv_langfr", "libertarian", "woke", "laicite", "immigration", "lien3")
#' party_scales <- compute_party_scales(aggregated_issue_positions,
#'                                      factanal_object,
#'                                      scale_names)
#' print(party_scales)
compute_party_scales <- function(aggregated_issue_positions,
                                 factanal_object,
                                 scale_names) {
  ## put it into wide format for potgrowth::compute_scale_scores
  wide <- aggregated_issue_positions %>%
    tidyr::pivot_wider(., names_from = "VARIABLE",
                       values_from = "position") %>%
    dplyr::mutate(party = factor(party, levels = unique(aggregated_issue_positions$party))) %>%
    dplyr::arrange(party)
  ## check if rownames from df are in the same exact order as wide$party
  if (sum((rownames(df) != wide$party)) > 0){
    stop(message("Rownames from df are not in the same order as the party column in wide."))
  }
  party_scales_matrix <- sapply(X = 1:length(scale_names),
                                FUN = compute_scale_scores,
                                factanal_object = factanal_object,
                                survey_data = wide)
  rownames(party_scales_matrix) <- unique(aggregated_issue_positions$party)
  colnames(party_scales_matrix) <- paste0("scale_", scale_names)
  return(party_scales_matrix)
}


#' Compute Attitude Gaps Between Party Positions and Survey Data
#'
#' Calculates the attitude gaps between party positions on various scales and individual responses
#' in survey data. This function allows for a comparison of positions on key political or ideological
#' scales between parties and survey respondents.
#'
#' @param party_scales_matrix A matrix containing the party positions on different scales.
#'        This is expected to be the output from `potgrowth::compute_party_scales`.
#' @param survey_data A data frame or list where each element/column represents respondent scores
#'        on the same scales as those in `party_scales_matrix`.
#' @param output Character string specifying the output format.
#'        If "cbind", the function returns the `survey_data` combined with the calculated attitude gaps.
#'        Defaults to "cbind".
#'
#' @return Depending on the `output` parameter, this function returns a data frame with the original
#'         `survey_data` combined with the calculated attitude gaps as additional columns.
#'
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stats setNames
#' @examples
#' # Assuming party_scales_matrix and survey_data are already defined:
#' gaps_df <- compute_attitude_gaps(party_scales_matrix, survey_data)
#' print(gaps_df)
#'
#' @export
compute_attitude_gaps <- function(
    party_scales_matrix,
    survey_data,
    output = "cbind"
){
  if (sum(!(colnames(party_scales_matrix) %in% names(survey_data))) > 0){
    stop(message("Scales from party_scales_matrix are not in survey_data."))
  }
  parties <- rownames(party_scales_matrix)
  scales <- colnames(party_scales_matrix)
  gaps_list <- lapply(
    X = parties,
    FUN = function(party){
      gaps_matrix <- sapply(
        X = scales,
        FUN = function(scale){
          matrix <- survey_data[[scale]] - party_scales_matrix[party, scale]
          return(matrix)
        }
      )
      colnames(gaps_matrix) <- paste0("attitudegap_", party, "_", scales)
      return(gaps_matrix)
    }
  )
  names(gaps_list) <- parties
  if (output == "cbind"){
    df_gaps <- cbind(survey_data, do.call(cbind, gaps_list))
    return(df_gaps)
  }
}



#' Retrieve Relative Confidence Indices (RCIs) for Parties
#'
#' This function extracts the Relative Confidence Indices (RCIs) for specified parties from a dataset. The RCI measures the confidence level relative to other indices.
#'
#' @param data A dataframe containing the dataset from which RCIs are to be retrieved.
#' @param rci_columns_prefix A string specifying the prefix of the columns in `data` that contain RCI values. Defaults to "irc_".
#' @param parties A vector of strings specifying the parties for which RCIs are to be retrieved. This parameter defaults to `potgrowth::qc_parties`, assuming `potgrowth` package contains a list of Quebec parties.
#' @return A matrix containing the RCI values for the specified parties.
#' @export
#' @examples
#' rcis <- retrieve_rcis(my_data, "irc_", c("party1", "party2"))
retrieve_rcis <- function(data,
                          rci_columns_prefix = "irc_",
                          parties = potgrowth::qc_parties){
  rcis <- sapply(parties,
                 FUN = function(x) data[, paste0(rci_columns_prefix, x)])
  return(rcis)
}

#' Retrieve Attitude Gaps for Parties
#'
#' This function calculates the attitude gaps for specified parties within given scales, excluding optionally the left-right scale. An attitude gap measures the difference in attitudes across different dimensions.
#'
#' @param data A dataframe containing the dataset from which attitude gaps are to be calculated.
#' @param attitudegap_columns_prefix A string specifying the prefix of the columns in `data` related to attitude gaps.
#' @param scale_columns_prefix A string specifying the prefix for scale columns in `data` from which the attitude gaps are calculated. Defaults to "scale".
#' @param parties A vector of strings specifying the parties for which attitude gaps are to be calculated.
#' @param remove_scale_leftRight A logical indicating whether to exclude the left-right scale from calculations. Defaults to TRUE.
#' @return A list of matrices, each representing the attitude gaps for a row in `data`, structured by the specified scales and parties.
#' @export
#' @examples
#' attitude_gaps <- retrieve_attitudegaps(my_data, "attitudegap_", "scale", c("party1", "party2"), TRUE)
retrieve_attitudegaps <- function(data,
                                  attitudegap_columns_prefix = "attitudegap_",
                                  scale_columns_prefix = "scale",
                                  parties = potgrowth::qc_parties,
                                  remove_scale_leftRight = TRUE){
  scales <- names(data %>% select(starts_with(scale_columns_prefix)))
  if (isTRUE(remove_scale_leftRight)){
    scales <- scales[scales != "scale_leftRight"]
  }
  message("lapply starting, you can expect to wait ~1.7 seconds by 1000 respondents.")
  message(paste0("   So for ", nrow(data), " respondents, you can expect: ~",
                 round(1.7 * (nrow(data) / 1000)), " seconds"))
  start <- Sys.time()
  gaps <- lapply(X = 1:nrow(data), function(row_index) {
    gaps <- sapply(X = scales, function(scale) {
      gaps <- 1- abs(data[row_index, paste0(attitudegap_columns_prefix, parties, "_", scale)])
      return(gaps)
    })
    colnames <- colnames(gaps)
    gaps <- matrix(unlist(gaps), nrow=5, byrow=TRUE)
    rownames(gaps) <- parties
    colnames(gaps) <- colnames
    return(gaps)
  })
  elapsed <- Sys.time() - start
  elapsed_secs <- as.numeric(elapsed, units = "secs")
  message(paste0("lapply done in ", elapsed_secs, " seconds."))
  return(gaps)
}

#' Compute Respondents' Saliency on Scales and Parties
#'
#' This function computes the correlation between Relative Confidence Indices (RCIs) and attitude gaps for respondents, indicating the saliency of different issues among different parties.
#'
#' @param data A dataframe containing the dataset to be analyzed.
#' @param rci_columns_prefix A string specifying the prefix of the RCI columns in `data`.
#' @param attitudegap_columns_prefix A string specifying the prefix of the attitude gap columns in `data`.
#' @param scale_columns_prefix A string specifying the prefix for scale columns in `data` from which the attitude gaps are calculated.
#' @param remove_scale_leftRight A logical indicating whether to exclude the left-right scale from calculations.
#' @param parties A vector of strings specifying the parties for which the analysis is to be performed.
#' @return A matrix with the correlation coefficients between RCIs and attitude gaps for each respondent, indicating the saliency of issues.
#' @export
#' @examples
#' saliency <- compute_respondents_saliency(my_data, "irc_", "attitudegap_", "scale", TRUE, c("party1", "party2"))
compute_respondents_saliency <- function(data,
                                         rci_columns_prefix = "irc_",
                                         attitudegap_columns_prefix = "attitudegap_",
                                         scale_columns_prefix = "scale",
                                         remove_scale_leftRight = TRUE,
                                         parties = potgrowth::qc_parties){
  rcis <- retrieve_rcis(
    data = data,
    rci_columns_prefix = rci_columns_prefix,
    parties = parties
  )
  attitudegaps <-
    retrieve_attitudegaps(
      data = data,
      attitudegap_columns_prefix = attitudegap_columns_prefix,
      scale_columns_prefix = scale_columns_prefix,
      parties = parties,
      remove_scale_leftRight = remove_scale_leftRight
    )
  output <- sapply(
    X = 1:nrow(data),
    FUN = function(x) {
      rcisx <- rcis[x,]
      attitudegapsx <- attitudegaps[[x]]
      cors <- cor(x = rcisx,
                  y = attitudegapsx)[1, ]
      return(cors)
    }
  )
  output <- t(output)
  return(output)
}


