#' Perform Fisher's Method for combining p-values
#'
#' \code{FisherMethodPValue} combines p-values using Fisher's method.
#'
#' Function for combining p-values by performing Fisher's method, which combines
#' p-values to a statistic:
#' \deqn{\chi_{2k}^k = -2 \sum_{i = 1}^{k} ln(p_i)}
#'
#' @param pvals A matrix or data.frame containing the p-values from the single
#'   tests
#' @param zero.sub Replacement for p-values of 0
#' @param na.rm Logical indicating whether NA values should be removed from the
#'   analysis.
#' @return A numeric vector with combined p-value
#' @examples
#' \dontrun{
#' pp <- matrix(c(runif(20),c(0.001,0.02,0.03,0.001)), ncol=4)
#' FisherMethodPValue(pp)
#' }
#' @export

FisherMethodPValue <- function (pvals, zero.sub = 1e-05, na.rm = FALSE) {
  stopifnot(all(pvals >= 0, na.rm = TRUE) & all(pvals <= 1, na.rm = TRUE))
  stopifnot(zero.sub >= 0 & zero.sub <= 1 || length(zero.sub) != 1)
  if (na.rm) {pvals <- pvals[!is.na(pvals)]}
  if (length(pvals) == 0) {
    return(as.numeric(NA))
  } else {
    pvals[pvals == 0] <- zero.sub
    S = -2 * sum(log(pvals))
    num.p = length(pvals)
    p.value <- 1 - pchisq(S, df = 2 * num.p)
    return(p.value)
  }
}
