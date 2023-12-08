#' @title Gradient synchronization (GS)
#' @description Estimate the gradient synchronization (GS) for 
#' a pair of curves observed on the same grid.
#' @param t A vector of support grid in a strictly increasing order. 
#' If functions are not supported on \eqn{[0,1]}, \code{t} will be normalized 
#' to \eqn{[0,1]} by \code{(t-min(t)) / diff(range(t))}.
#' @param x,y The values of the two functions evaluated on \code{t}.
#' @return A scalar holding the gradient synchronization.
#' @export
#' 
getGS <- function (t,x,y) {
  if ( is.unsorted(t) ) stop ( 't is not monotonic.' )
  dt <- diff(t)
  if ( any( dt == 0 ) ) stop ( 't includes repeated values.' )
  dx <- diff(x); dy <- diff(y)
  sum( sign( dx * dy ) * dt ) / diff( range(t) )
}