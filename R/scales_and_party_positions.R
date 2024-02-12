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
    mutate(party = factor(party, levels = unique(aggregated_issue_positions$party))) %>%
    arrange(party)
  ## check if rownames from df are in the same exact order as wide$party
  if (sum((rownames(df) != wide$party)) > 0){
    stop(message("Rownames from df are not in the same order as the party column in wide."))
  }
  party_scales_matrix <- sapply(X = 1:length(scale_names),
                                FUN = potgrowth::compute_scale_scores,
                                factanal_object = factanal_object,
                                survey_data = wide)
  rownames(party_scales_matrix) <- unique(aggregated_issue_positions$party)
  colnames(party_scales_matrix) <- paste0("scale_", scale_names)
  return(party_scales_matrix)
}



