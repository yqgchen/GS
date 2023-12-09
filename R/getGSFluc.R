#' @title Gradient synchronization fluctuation (GSF)
#' @description Estimate the gradient synchronization fluctuation (GSF) for 
#' a pair of curves observed on the same grid.
#' @param t A vector of support grid in a strictly increasing order. 
#' @param x,y The values of the two functions evaluated on \code{t}.
#' @return A scalar holding the gradient synchronization fluctuation.
#' @export
#' 
getGSFluc <- function (t,x,y) {
  if ( is.unsorted(t) ) stop ( 't is not monotonic.' )
  dt <- diff(t)
  if ( any( dt == 0 ) ) stop ( 't includes repeated values.' )
  dxdt <- diff(x) / dt
  dydt <- diff(y) / dt
  m <- length(dxdt)
  sum( dxdt[-1] * dxdt[-m] * dydt[-1] * dydt[-m] < 0 ) +
    sum( abs( dxdt * dydt ) == 0 )
}