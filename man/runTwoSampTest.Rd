% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runTwoSampTest.R
\name{runTwoSampTest}
\alias{runTwoSampTest}
\title{Two-sample Wilcoxon rank sum test}
\usage{
runTwoSampTest(res, fun = absmean, g1 = 1, g2 = 2)
}
\arguments{
\item{res}{A list of association measures from different groups to be compared, 
each field corresponds to one group and holds an \eqn{n}-by-\eqn{p} matrix, 
where \eqn{n} is the number of subjects which may vary across fields, and \eqn{p} 
is the number of pairs of functions, i.e., \eqn{p=r(r-1)/2} if there are \eqn{r} functions.}

\item{fun}{A function to aggregate results across pairs of functions, 
such as \code{\link[base]{mean}}, \code{\link{absmean}}, etc.}

\item{g1, g2}{The indices or names of the fields in \code{res} corresponding to 
the two groups to be compared.}
}
\description{
Perform a two-sample Wilcoxon rank sum test for a summary statistic 
over pairs of functions using \code{\link[stats]{wilcox.test}}.
}
