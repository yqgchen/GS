#' @title Kruskal--Wallis rank sum test (ANOVA)
#' @description Perform a Kruskal--Wallis rank sum test (ANOVA) for a summary statistic 
#' over pairs of functions using \code{\link[stats]{kruskal.test}}.
#' @param res A list of association measures from different groups to be compared, 
#' each field corresponds to one group and holds an \eqn{n}-by-\eqn{p} matrix, 
#' where \eqn{n} is the number of subjects which may vary across fields, and \eqn{p} 
#' is the number of pairs of functions, i.e., \eqn{p=r(r-1)/2} if there are \eqn{r} functions.
#' @param fun A function to aggregate results across pairs of functions, 
#' such as \code{\link[base]{mean}}, \code{\link{absmean}}, etc.
#' @importFrom stats kruskal.test
#' @export
#' 
runANOVA <- function ( res, fun = absmean ) {
  label <- names(res)
  if (is.null(label)) label <- factor(seq_along(res))
  Lsum <- lapply( seq_along(res), function(i) {
    data.frame(
      group = rep(label[i],nrow(res[[i]])),
      x = apply( res[[i]], 1, fun )
    )
  })
  df <- do.call(rbind, Lsum)
  with( df, kruskal.test( x ~ group ) )
}
