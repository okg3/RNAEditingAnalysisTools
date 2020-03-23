#' Get ggplot default colors
#' 
#' \code{gg_color_hue} returns n colors using ggplot default palette 
#' 
#' @param n number of colors to generate 
#' @return character vector specifying colors by hexadecimal code
#' @examples 
#' \dontrun{
#' barplot(c(a = 2,b = 4, c= 3), col = gg_color_hue(3))
#' }
#' @export
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
