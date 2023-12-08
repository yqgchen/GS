#'@title Mean magnitude
#'@description Compute the arithmetic mean of the absolute values.
#'@param x A numeric vector,
#'@return A scalar holding the mean magnitude.
#'@export
absmean <- function (x) { 
  mean(abs(x))
}