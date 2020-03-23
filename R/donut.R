#' Donut Chart
#'
#' \code{donut} draws a donut chart
#'
#' Draw a donut chart.
#'
#' @param x a vector of non-negative numerical quantities. The values in x are
#'   displayed as the areas of pie slices.
#' @param labels one or more expressions or character strings giving names for
#'   the slices. Other objects are coerced by as.graphicsAnnot. For empty or NA
#'   (after coercion to character) labels, no label nor pointing line is drawn.
#' @param edges the circular outline of the pie is approximated by a polygon
#'   with this many edges.
#' @param outer.radius the pie is drawn centered in a square box whose sides
#'   range from -1 to 1. If the character strings labeling the slices are long
#'   it may be necessary to use a smaller radius.
#' @param inner.radius radius of donut hole in center
#' @param clockwise logical indicating if slices are drawn clockwise or counter
#'   clockwise (i.e., mathematically positive direction), the latter is default.
#' @param int.angle number specifying the starting angle (in degrees) for the
#'   slices. Defaults to 0 (i.e., '3 o'clock') unless clockwise is true where
#'   init.angle defaults to 90 (degrees), (i.e., '12 o'clock').
#' @param density the density of shading lines, in lines per inch. The default
#'   value of NULL means that no shading lines are drawn. Non-positive values of
#'   density also inhibit the drawing of shading lines.
#' @param angle the slope of shading lines, given as an angle in degrees
#'   (counter-clockwise).
#' @param col a vector of colors to be used in filling or shading the slices. If
#'   missing a set of 6 pastel colours is used, unless density is specified when
#'   par("fg") is used.
#' @param border,lty (possibly vectors) arguments passed to polygon which draws
#'   each slice.
#' @param main an overall title for the plot.
#' @param ... graphical parameters can be given as arguments to donut. They will
#'   affect the main title and labels only.
#' @references doughnut plot script from https://magesblog.com/
#' @examples
#' \dontrun{
#' donut.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
#' names(donut.sales) <- c("Blueberry", "Cherry",
#'                       "Apple", "Boston Cream", "Other", "Vanilla Cream")
#' donut(donut.sales) # default colours
#' donut(donut.sales, col = c("purple", "violetred1", "green3",
#'                        "cornsilk", "cyan", "white"))
#' donut(donut.sales, col = gray(seq(0.4, 1.0, length = 6)))
#' donut(donut.sales, density = 10, angle = 15 + 10 * 1:6)
#' donut(donut.sales, clockwise = TRUE, main = "donut(*, clockwise = TRUE)")
#' segments(0, 0, 0, 1, col = "red", lwd = 2)
#' text(0, 1, "init.angle = 90", col = "red")
#'
#' n <- 200
#' donut(rep(1, n), labels = "", col = rainbow(n), border = NA,
#'     main = "donut(*, labels=\"\", col=rainbow(n), border=NA,..")
#' }
#' @export

donut <- function (x, labels = names(x), edges = 200, outer.radius = 0.8,
                   inner.radius=0.6, clockwise = FALSE,
                   init.angle = if (clockwise) 90 else 0, density = NULL,
                   angle = 45, col = NULL, border = FALSE, lty = NULL,
                   main = NULL, ...) {
  if (!is.numeric(x) || any(is.na(x) | x < 0)){
    stop("'x' values must be positive.")
  }
  if (is.null(labels)){
    labels <- as.character(seq_along(x))
  } else {
    labels <- as.graphicsAnnot(labels)
  }
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]){
    xlim <- (pin[1L]/pin[2L]) * xlim
  } else {
    ylim <- (pin[2L]/pin[1L]) * ylim
  }
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)){
    if (is.null(density)){
      col <- gg_color_hue(nx)
    }
  } else {
    par("fg")
  }
  col <- rep(col, length.out = nx)
  border <- rep(border, length.out = nx)
  if(!is.null(lty)) {
    lty <- rep(lty, length.out = nx)
  }
  angle <- rep(angle, length.out = nx)
  if(!is.null(density)) {
    density <- rep(density, length.out = nx)
  }
  twopi <- ifelse(clockwise, -2 * pi, 2 * pi)
  t2xy <- function(t, radius) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p),
         y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
              outer.radius)
    polygon(c(P$x, 0), c(P$y, 0), density = density[i],
            angle = angle[i], border = border[i],
            col = col[i], lty = lty[i])
    Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
      text(1.1 * Pout$x, 1.1 * Pout$y, labels[i],
           xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0),
           ...)
    }
    ## Add white disc          
    Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                inner.radius)
    polygon(Pin$x, Pin$y, density = density[i],
            angle = angle[i], border = border[i],
            col = "white", lty = lty[i])
  }
  
  title(main = main, ...)
  invisible(NULL)
}
