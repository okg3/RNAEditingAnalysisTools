#' Get Consensus Substitution Type for RNA Editing Event
#'
#' \code{GetSubType} identifies the most common substitution type across samples
#' for each editing event
#'
#' @param x a matrix with substitution type observed at each editing event for
#'   each individual (AllSubs)
#' @return A character vector with most common substitution type for each
#'   editing event
#' @examples
#' \dontrun{
#' subtypes <- GetSubType(RNAEdDataQCed$AllSubs)
#' table(subtypes)
#' }
#' @export

GetSubType <- function(x){
  vapply(1:nrow(x), FUN = function(i){
    result <- names(which.max(table(x[i,])))
    if(is.null(result)){
      result <- as.character(NA)
    }
    result
  }, FUN.VALUE = character(1))
}
