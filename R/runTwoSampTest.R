#' @title Two-sample Wilcoxon rank sum test
#' @description Perform a two-sample Wilcoxon rank sum test for a summary statistic 
#' over pairs of functions using \code{\link[stats]{wilcox.test}}.
#' @param res A list of association measures from different groups to be compared, 
#' each field corresponds to one group and holds an \eqn{n}-by-\eqn{p} matrix, 
#' where \eqn{n} is the number of subjects which may vary across fields, and \eqn{p} 
#' is the number of pairs of functions, i.e., \eqn{p=r(r-1)/2} if there are \eqn{r} functions.
#' @param fun A function to aggregate results across pairs of functions, 
#' such as \code{\link[base]{mean}}, \code{\link{absmean}}, etc.
#' @param g1,g2 The indices or names of the fields in \code{res} corresponding to 
#' the two groups to be compared.
#' @importFrom stats wilcox.test
#' @export
#' 
runTwoSampTest <- function ( res, fun = absmean, g1 = 1, g2 = 2 ) {
  wilcox.test( apply(res[[g1]],1,fun), apply(res[[g2]],1,fun) )
}
