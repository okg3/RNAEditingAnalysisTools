#' Calculate number of edited reads from frequency and coverage
#'
#' \code{GetEditedReads} calculates the number of edited reads using the
#' Frequency and Coverage-q25 matrices from the RNA Editing data object
#'
#' The input should be a list object which includes matrices named "Frequency"
#' and "Coverage-q25".
#'
#' @param x An RNA editing data object which includes Frequency matrix and
#'   Coverage-q25 matrix at minimum
#' @return A matrix with total number of edited reads for each edited site by
#'   individual
#' @examples
#' \dontrun{
#' head(GetEditedReads(RNAEdData))
#' }
#' @export

GetEditedReads <- function(x){
  x$EditedReads <- round(x$Frequency * x$`Coverage-q25`, 0)
  x
}
