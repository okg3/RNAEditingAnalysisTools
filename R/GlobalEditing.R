#' Get Global Editing Values
#'
#' \code{GlobalEditing} returns global editing values for each sample.
#'
#' The input should be a list object which includes matrices named "Frequency"
#' and "Coverage-q25". The function will return a vector, matrix, or list of
#' vectors or matrices with options to report the following:
#'
#' \describe{
#'     \item{Proportion}{Total proportion of edited reads over reads mapping to edited sites (EditedReads / Coverage)}
#'     \item{Count}{Total number of sites with editing present}
#'     \item{Coverage}{Total number of sites with coverage (recommended for normalization of counts above)}
#' }
#'
#' By default, calculates global values for all editing events combined. To
#' split by region, subtype, or other annotation, supply name of column in
#' Annotation data.frame to be used to split data.
#'
#' @param x An RNA editing data object which includes Annotation dataframe,
#'   Frequency matrix, and Coverage-q25 matrix at minimum
#' @param type A character vector with values "proportion", "count", and/or
#'   "covered" specifying global editing measure to return. By default, returns
#'   all three.
#' @param by A character vector with name of column in x$Annotation dataframe by
#'   which to split editing data
#' @return A vector, matrix, or list of vectors or matrices with global editing
#'   values.
#' @examples
#' \dontrun{
#' GlobalEditing(RNAEdDataQCed)
#' GlobalEditing(RNAEdDataQCed, type = "proportion")
#' GlobalEditing(RNAEdDataQCed, type = "count", by = "SubType")
#' }
#' @export

GlobalEditing <- function(x, type = c("proportion", "count", "covered"),
                          by = NULL){
  result <- lapply(type, function(ty){
    if(is.null(by)){
      annot <- list(x$Annotation)
    } else {
      annot <- split(x$Annotation, by = by)
    }
    if(!("EditedReads" %in% names(x))){
      x <- GetEditedReads(x)
    }
    if(ty == "proportion"){
      count <- lapply(annot, function(a){
        if (length(a$id) > 1){
          colSums(x$EditedReads[a$id,], na.rm = TRUE) /
            colSums(x$`Coverage-q25`[a$id, ], na.rm = TRUE)
        } else {
          x$EditedReads[a$id,] / x$`Coverage-q25`[a$id, ]
        }
      })
    } else if (ty == "count"){
      count <- lapply(annot, function(a){
        if (length(a$id) > 1){
          colSums(x$EditedReads[a$id,] > 0, na.rm = TRUE)
        } else {
          as.integer(x$EditedReads[a$id,] > 0)
        }
      })
    } else if (ty == "covered"){
      count <- lapply(annot, function(a){
        if (length(a$id) > 1){
          colSums(x$`Coverage-q25`[a$id,] > 0, na.rm = TRUE)
        } else {
          as.integer(x$`Coverage-q25`[a$id,] > 0)
        }
      })
    } else {
      stop("Error: Illegal function call")
    }
    count <- Reduce(rbind, count)
    rownames(count) <- names(annot)
    count
  })
  if(length(result) == 1){
    result[[1]]
  } else {
    names(result) <- type
    result
  }
}
