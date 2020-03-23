#' Identify Correct Strand for Editing Events
#'
#' \code{IdentifyCorrectStrand} is an internal function for identifying the
#' correct strand for editing events
#'
#' @param annot Annotation dataframe
#' @param splitBy Character vector giving name of column by which to group
#'   editing events
#' @param values Character vector with unique values to include
#' @return A matrix with value name, correct strand for A, and correct strand
#'   for C.

IdentifyCorrectStrand <- function(annot, splitBy, values){
  out <- vapply(values, function(v){
    tmp <- annot[get(splitBy) == v]
    tmp_table <- table(tmp$SubType, tmp$Strand)
    
    if (!("0" %in% colnames(tmp_table))){
      tmp_table <- cbind(tmp_table, "0" = rep.int(0, nrow(tmp_table)))
    }
    if (!("1" %in% colnames(tmp_table))){
      tmp_table <- cbind(tmp_table, "1" = rep.int(0, nrow(tmp_table)))
    }
    if ("TC" %in% rownames(tmp_table)){
      if ("AG" %in% rownames(tmp_table)){
        strand_A <- colnames(tmp_table)[
          max.col(tmp_table)[rownames(tmp_table) == "AG"]]
      } else {
        strand_A <- colnames(tmp_table)[
          !(1:2 %in% max.col(tmp_table)[rownames(tmp_table) == "TC"])]
      }
    } else if ("AG" %in% rownames(tmp_table)){
      strand_A <- colnames(tmp_table)[
        max.col(tmp_table)[rownames(tmp_table) == "AG"]]
    } else {
      strand_A <- NA
    }
    
    if ("GA" %in% rownames(tmp_table)){
      if ("CT" %in% rownames(tmp_table)){
        strand_C <- colnames(tmp_table)[
          max.col(tmp_table)[rownames(tmp_table) == "CT"]]
      } else {
        strand_C <- colnames(tmp_table)[
          !(1:2 %in% max.col(tmp_table)[rownames(tmp_table) == "GA"])]
      }
    } else if ("CT" %in% rownames(tmp_table)){
      strand_C <- colnames(tmp_table)[
        max.col(tmp_table)[rownames(tmp_table) == "CT"]]
    } else {
      strand_C <- NA
    }
    strand_A <- as.character(strand_A)
    strand_C <- as.character(strand_C)
    c(v,strand_A, strand_C)
  }, FUN.VALUE = character(3))
  out <- t(out)
  colnames(out) <- c("value", "strand_A", "strand_C")
  out
}
