#' Categorize RNA Editing Events by Alu and Non-Alu Repeats
#'
#' \code{GetAluRepeat} categorizes RNA editing events as falling within Alu
#' repeats, non-Alu repetitive elements, and non-repetitive regions
#'
#' @param x a vector giving UCSC repeat masking data
#' @return A character vector with values "Alu", "Non-Alu Repeat", and
#'   "Non-Repetitive Element"
#' @examples
#' \dontrun{
#' alu <- GetAluRepeat(RNAEdDataQCed$Annotation$RepMask_gid)
#' table(alu)
#' }
#' @export

GetAluRepeat <- function(x){
  vapply(x, FUN = function(y){
    if(grepl("Alu", y)){
      return("Alu")
    } else if (is.na(y)){
      return("Non-Repetitive Element")
    } else {
      return("Non-Alu Repeat")
    }
  }, FUN.VALUE = character(1))
}
