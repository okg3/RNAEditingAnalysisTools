#' Correct Strand Calling for RNA Editing Events
#'
#' \code{StrandCorrection} identifies the correct strand for RNA editing calling
#' and corrects data
#'
#' @param x An RNA Editing data list object - first item of list must be
#'   Annotation data.frame
#' @param genes A character string giving the name of the column in Annotation
#'   data.frame with RefSeq gene annotations for gene-level strand correction
#' @return An RNA Editing data object with strand correction
#' @examples
#' \dontrun{
#' corrected <- StrandCorrection(RNAEdData, genes = "RefSeq_gid")
#' }
#' @export

StrandCorrection <- function(x, genes = NULL){
  # dataset should be merged dataset with all info
  if(missing(x)){
    stop("Error: x missing with no default\n")
  } else if(!is.list(x)){
    stop("Error: x must be list object\n")
  }
  x[[1]] <- data.table::setDT(x[[1]])
  if(!("SubType" %in% colnames(x[[1]]))){
    x[[1]] <- x[[1]][, SubType := GetSubType(x[["AllSubs"]])]
  }

  # Check for duplicates
  x[[1]] <- x[[1]][, coord := paste(Region, Position, sep = "_")]
  dup_annot <- unique(x[[1]][duplicated(coord),][["coord"]])
  correctStrand <- IdentifyCorrectStrand(x[[1]], "coord", dup_annot)
  x[[1]] <- merge(x[[1]], correctStrand, by.x = "coord", by.y = "value",
                 all.x = TRUE, all.y = FALSE)
  x[[1]] <- x[[1]][, coord := NULL]
  # genes character or numeric vector indicating column in annot with gene names
  # or gene name vector itself (length must == nrow annot)
  if(!is.null(genes)){
    if(length(genes) > 1){
      if(length(genes) != nrow(x[[1]])){
        stop(paste0("Error: length of genes vector does not equal ",
                    "number of rows in annotation data.frame"))
      }
      x[[1]] <- x[[1]][ , geneID := genes]
      genes <- "geneID"
    } else if (is.numeric(genes)){
      genes <- colnames(x[[1]])[genes]
    }
    genes_annot <- unique(x[[1]][
      get(genes) != "-" & !is.na(get(genes))][[genes]])
    colOrder <- colnames(x[[1]])
    correctStrand <- IdentifyCorrectStrand(x[[1]], genes, genes_annot)
    x[[1]] <- merge(x[[1]], correctStrand, by.x = genes, by.y = "value",
                   all.x = TRUE, all.y = FALSE)
    x[[1]] <- setcolorder(
      x[[1]], c(colOrder[colOrder %in% colnames(x[[1]])],
               colnames(x[[1]])[!(colnames(x[[1]]) %in% colOrder)]))
    colstomerge <- colnames(x[[1]])[grep("strand_.*", colnames(x[[1]]))]
    x[[1]] <- x[[1]][
      , strand_A := MultiDataAnalysis::coalesce(list(strand_A.x, strand_A.y))]
    x[[1]] <- x[[1]][
      , strand_C := MultiDataAnalysis::coalesce(list(strand_C.x, strand_C.y))]
    x[[1]] <- x[[1]][ , .SD, .SDcols = !colstomerge]
  }
  x[[1]] <- x[[1]][
    SubType == "TC" & !is.na(strand_A),
    c("Reference", "SubType") :=
      list("A", "AG")
    ]
  x[[1]] <- x[[1]][
    !is.na(strand_A), Strand := as.integer(strand_A)
    ]
  x[[1]] <- x[[1]][
    SubType == "GA" & !is.na(strand_C),
    c("Reference", "SubType") :=
      list("C", "CT")
    ]
  x[[1]] <- x[[1]][
    !is.na(strand_C), Strand := as.integer(strand_C)
    ]
  x[[1]] <- x[[1]][, c("strand_A", "strand_C") := NULL]

  colnames(x[[1]])[1] <- "id"
  x[[1]] <- x[[1]][, id := paste(Region, Position, Reference, Strand, sep = "_")]
  x[[1]] <- x[[1]][order(id)]

  # now merge rows with duplicate ids
  dup_ids <- unique(x[[1]][duplicated(id)][["id"]])
  combFuncs <- StrandCorrectionFuncs(x)
  for (dID in dup_ids){
    for(j in 1:ncol(x[[2]])){
      for(func in combFuncs){
        eval(parse(text = func))
      }
    }
  }
  x[-1] <- lapply(x[-1], function(m){
    m[!duplicated(x[[1]][[1]]),]
  })
  x[[1]] <- unique(x[[1]])
  x[-1] <- lapply(x[-1], function(m){
    rownames(m) <- x[[1]][[1]]
    m
  })
  x
}
