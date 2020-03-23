#' Return number of databases reporting RNA editing event
#'
#' \code{CheckDatabase} returns the number of databases reporting an RNA editing
#' event
#'
#' @param ... one or more vectors giving database annotations for each RNA
#'   editing event
#' @return A numeric vector with database counts
#' @examples
#' \dontrun{
#' db <- CheckDatabase(
#' RNAEdDataQCed$Annotation$RADAR_gid,
#' RNAEdDataQCed$Annotation$DARNED_gid,
#' RNAEdDataQCed$Annotation$REDIportal_gid
#' )
#' table(db)
#' }
#' @export

CheckDatabase <- function(...){
  result <- lapply(list(...), function(x){ifelse(is.na(x), 0,1)})
  Reduce(`+`, result)
}
