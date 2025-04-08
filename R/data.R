#' Clean Issues
#'
#' A named vector mapping short issue codes to their descriptions.
#'
#' @format A named vector of 8 elements.
#' @usage data(clean_issues)
#' @export
clean_issues <- c("nationalisme" = "Nationalisme/\nSouveraineté",
                  "lang" = "Protection\nlangue française",
                  "laic" = "Laïcité\nde l'État",
                  "immig" = "Immigration",
                  "newleft" = "Nouvelle gauche",
                  "liberty" = "Libertarisme",
                  "lien3" = "Troisième lien",
                  "enviro" = "Environnement")

#' Issues
#'
#' A vector of issue codes.
#'
#' @format A character vector of 8 elements.
#' @usage data(issues)
#' @export
issues <- names(clean_issues)

#' QC Provincial Parties
#'
#' A vector of QC provincial party codes.
#'
#' @format A character vector of 5 elements.
#' @usage data(qc_parties)
#' @export
qc_parties <- c("CAQ", "PLQ", "PQ", "QS", "PCQ")

#' QC Parties' Colors
#'
#' A named vector mapping party codes to their colors.
#'
#' @format A named vector of 5 elements.
#' @usage data(party_colors)
#' @export
qc_party_colors <- c("CAQ" = "#00cccc", "PLQ" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "PCQ" = "purple")


#' Canada Federal Parties
#'
#' A vector of Canada federal party codes.
#'
#' @format A character vector of 6 elements.
#' @usage data(can_parties)
#' @export
can_parties <- c(
  "lpc" = "PLC",
  "cpc" = "PCC",
  "bq" = "BQ",
  "ndp" = "NPD",
  "gpc" = "PVC",
  "ppc" = "PPC"
)

#' Canada Parties' Colors
#'
#' A named vector mapping party codes to their colors.
#'
#' @format A named vector of 5 elements.
#' @usage data(can_party_colors)
#' @export
can_party_colors <- c(
  "bq" = "#87CEFA",
  "lpc" = "#d71b1f",
  "cpc" = "#1d4881",
  "ndp" = "#f89922",
  "gpc" = "#3d9b35",
  "ppc" = "#442d7b"
)


