#' Categorize RNA Editing Events by Gene Region
#'
#' \code{GetGeneRegion} categorizes RNA editing events by genomic region
#' according to RefSeq annotation
#'
#' @param x a vector giving UCSC RefSeq annotation
#' @return A character vector with values "3UTR", "5UTR", "CDS", "Exon",
#'   "Intron", and "Intergenic"
#' @examples
#' \dontrun{
#' regions <- GetGeneRegion(RNAEdDataQCed$Annotation$RefSeq_feat)
#' table(regions)
#' }
#' @export


GetGeneRegion <- function(x){
  gene_regions_in_chr <- c("3UTR", "5UTR", "CDS", "exon", 
                           "transcript", "-")
  gene_regions_out_chr <- c("3UTR", "5UTR", "CDS", "Exon", 
                            "Intron", "Intergenic")
  vapply(x, FUN = function(y){
    if(is.na(y)){
      return("Intergenic")
    } else {
      for (i in 1:length(gene_regions_in_chr)){
        if(grepl(gene_regions_in_chr[i],y)){
          return(gene_regions_out_chr[i])
          break()
        }
      }
    }
  }, FUN.VALUE = character(1))
}