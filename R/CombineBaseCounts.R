#' Combine BaseCounts
#'
#' \code{CombineBaseCounts} sums a vector of base counts 
#' 
#' The input to CombineBaseCounts should be a vector of character strings in
#' REDItools BaseCount format: [A, C, G, T] where A, C, G, and T represent the
#' number of reads with As, Cs, Gs, and Ts mapping to that site, respectively
#' 
#' @param x character vector in REDItools BaseCount format
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return A string with aggregated base counts 
#' @examples
#' bc <- c("[6, 0, 4, 0]", "[2, 0, 8, 0]", NA)
#' CombineBaseCounts(bc, na.rm = TRUE)
#' @export

CombineBaseCounts <- function(x, na.rm = FALSE){
  if (na.rm) {
    x <- x[!is.na(x)]
    x <- x[!is.nan(x)]
  }
  if (length(x) == 0){
    return(as.character(NA))
  } else if (length(x) == 1){
    return(x)
  } else {
    x_num <- gsub("\\[|\\]", "", x)
    x_num <- strsplit(x_num, ", ")
    x_num <- lapply(x_num, as.numeric)
    x_sum <- Reduce(`+`, x_num)
    x <- paste0("[",paste(x_sum, collapse = ", "), "]")
    return(x)
  }
}
