#' Assemble fusionACS microdata (for AI use only)
#'
#' This function assembles microdata from a local fusionACS database, given a user's requested vintage, variables, and respondent type.
#'
#' @param variables Character vector specifying the names of survey variables to return. In addition to \code{vars}, the output always includes universal identifier variables (see Details).
#' @param year Scalar or numeric vector specifying the year(s) of ACS-PUMS microdata to use.
#' @param respondent Character. Whether to return "household" or "person" microdata (i.e. the type of survey respondent). When \code{respondent = "household"}, any person-level variables requested in \code{vars} are restricted to the head of household (i.e. reference person) value so that a single record can be merged to the requested household-level microdata. When \code{respondent = "person"}, any household-level variables requested in \code{vars} are replicated for each person within a household.
#' @return The path to a temporary file containing the output microdata.
#' @export

#-----

assembleAI <- function(variables,
                       year,
                       respondent) {

  # Call assemble() to return microdata in memory
  out <- assemble(variables = unlist(variables),
                  year = unlist(year),
                  respondent = unlist(respondent))

  # Save result to temporary file
  tf <- paste0(tempfile(), ".fst")
  fst::write_fst(out, path = tf, compress = 50)
  return(tf)

}
