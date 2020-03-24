#' Identify Functions to be Applied to RNA Editing Dataset for Strand Correction
#'
#' \code{StrandCorrectionFuncs} is an internal function that tests for the
#' presence of different matrix types in RNA editing data and lists appropriate
#' function to correct each
#'
#' @param x An RNA Editing data list object - first item of list must be
#'   Annotation data.frame
#' @return A character vector with functions to be parsed in loop in
#'   StrandCorrection()

StrandCorrectionFuncs <- function(x){
  combFuncs <- NULL
  wmMats <- matrix(c("MeanQ", "Frequency", "gMeanQ", "gFrequency",
                     "Coverage-q25", "Coverage-q25",
                     "gCoverage-q25", "gCoverage-q25"), ncol = 2)
  sMats <- c("Coverage-q25", "gCoverage-q25")
  bcMats <- c("BaseCount[A,C,G,T]", "gBaseCount[A,C,G,T]")
  cMats <- c("AllSubs", "gAllSubs")
  pMats <- "Pvalue"

  for(i in seq_len(nrow(wmMats))){
    if(wmMats[i,1] %in% names(x) & wmMats[i,2] %in% names(x)){
      combFuncs <- c(
        combFuncs,
        paste0(
          "x[[\"",wmMats[i,1],"\"]][x[[1]]$id == dID, j] <- weighted.mean(",
          "x[[\"",wmMats[i,1],"\"]][x[[1]]$id == dID, j], ",
          "x[[\"",wmMats[i,2],"\"]][x[[1]]$id == dID, j], ",
          "na.rm = TRUE)"
        )
      )
    } else if (wmMats[i,1] %in% names(x)){
      combFuncs <- c(
        combFuncs,
        paste0(
          "x[[\"",wmMats[i,1],"\"]][x[[1]]$id == dID, j] <- mean(",
          "x[[\"",wmMats[i,1],"\"]][x[[1]]$id == dID, j], na.rm = TRUE)"
        )
      )
    }
  }

  for(s in sMats){
    if(s %in% names(x)){
      combFuncs <- c(
        combFuncs,
        paste0(
          "x[[\"", s, "\"]][x[[1]]$id == dID, j] <- sum(",
          "x[[\"", s, "\"]][x[[1]]$id == dID, j], na.rm = TRUE)"
        )
      )
    }
  }

  for(bc in bcMats){
    if(bc %in% names(x)){
      combFuncs <- c(
        combFuncs,
        paste0(
          "x[[\"", bc, "\"]][x[[1]]$id == dID, j] <- CombineBaseCounts(",
          "x[[\"", bc, "\"]][x[[1]]$id == dID, j], na.rm = TRUE)"
        )
      )
    }
  }

  for(c in cMats){
    if(c %in% names(x)){
      combFuncs <- c(
        combFuncs,
        paste0(
          "x[[\"", c, "\"]][x[[1]]$id == dID, j] <- coalesce(",
          "x[[\"", c, "\"]][x[[1]]$id == dID, j])"
        )
      )
    }
  }

  for (p in pMats){
    if (p %in% names(x)){
      combFuncs <- c(
        combFuncs,
        paste0(
          "x[[\"", p, "\"]][x[[1]]$id == dID, j] <- FisherMethodPValue(",
          "x[[\"", p, "\"]][x[[1]]$id == dID, j], na.rm = TRUE)"
        )
      )
    }
  }

  return(combFuncs)
}
