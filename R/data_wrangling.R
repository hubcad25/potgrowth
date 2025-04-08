#' Compute Relative Confidence Index (RCI) from survey
#'
#' This function calculates the Relative Confidence Index (RCI) for each party's
#' vote probability within individuals, based on a given prefix. It reshapes the data,
#' identifies leading and trailing parties, and computes the difference with the
#' second-highest or maximum probability accordingly.
#'
#' @param df A data.frame or tibble containing an identifier column and vote probability columns.
#' @param prefix A character string specifying the prefix of the vote probability variables (e.g., `"dv_potgrowth_"`).
#' @param id_col A character string indicating the name of the identifier column (e.g., `"id"`).
#'
#' @return A tibble with one row per individual and one column per party's RCI score,
#'         prefixed with `"rci_"`, along with the identifier column.
#'
#' @examples
#' # compute_rci(df = df_pilote, prefix = "dv_potgrowth_", id_col = "id")
#'
#' @import dplyr
#' @import tidyr
#' @importFrom dplyr select starts_with all_of across group_by mutate case_when if_else ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @export
compute_rci <- function(df, prefix, id_col) {
  df |>
    dplyr::select(dplyr::all_of(id_col), dplyr::starts_with(prefix), -dplyr::starts_with(paste0(prefix, "Qc"))) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with(prefix),
      names_to = "party",
      names_prefix = prefix,
      values_to = "potgrowth"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::mutate(
      max_potgrowth = max(potgrowth, na.rm = TRUE),
      leader = as.integer(potgrowth == max_potgrowth),
      trailer = as.integer(potgrowth != max_potgrowth),
      n_leaders = sum(leader),
      potgrowth_trailers = dplyr::if_else(trailer == 1, potgrowth, NA_real_),
      second_potgrowth = dplyr::case_when(
        n_leaders == 1 ~ max(potgrowth_trailers, na.rm = TRUE),
        n_leaders >= 2 ~ max_potgrowth
      ),
      rci = dplyr::case_when(
        leader == 1 ~ potgrowth - second_potgrowth,
        trailer == 1 ~ potgrowth - max_potgrowth
      )
    ) |>
    dplyr::select(dplyr::all_of(id_col), party, rci) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_col),
      names_from = party,
      values_from = rci,
      names_prefix = "rci_"
    )
}


#' Parse Money Vector
#'
#' This function processes a vector of monetary values, parsing each value into a numeric form. The function handles
#' specific floor and ceiling values differently by using specified limits and an increment for ceilings.
#'
#' @param values A vector of monetary values as strings to be parsed.
#' @param sep A character string specifying the separator used in monetary ranges.
#' @param floor The floor value to be specifically handled.
#' @param ceiling The ceiling value to be specifically handled.
#' @param ceiling_increment A numeric value to increment ceilings by, defaults to 10000.
#'
#' @return A numeric vector where each element is the parsed and rounded mean of the corresponding input value.
#'
#' @examples
#' # Example usage:
#' # parse_money_vector(values = c("1000-2000", "floor", "ceiling"), sep = "-", floor = "floor", ceiling = "ceiling")
#'
#' @importFrom sondr parse_money_range
#' @export
parse_money_vector <- function(values, sep, floor, ceiling, ceiling_increment = 10000) {
  sapply(values, function(value) {
    if (is.na(value) || value == "") {
      return(NA)
    }
    if (value == floor) {
      round(mean(sondr::parse_money_range(value, limit = "floor")))
    } else if (value == ceiling) {
      round(mean(sondr::parse_money_range(value, limit = "ceiling", ceiling_increment = ceiling_increment)))
    } else {
      round(mean(sondr::parse_money_range(value, sep)))
    }
  })
}
