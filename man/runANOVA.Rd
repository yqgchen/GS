% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runANOVA.R
\name{runANOVA}
\alias{runANOVA}
\title{Kruskal--Wallis rank sum test (ANOVA)}
\usage{
runANOVA(res, fun = absmean)
}
\arguments{
\item{res}{A list of association measures from different groups to be compared, 
each field corresponds to one group and holds an \eqn{n}-by-\eqn{p} matrix, 
where \eqn{n} is the number of subjects which may vary across fields, and \eqn{p} 
is the number of pairs of functions, i.e., \eqn{p=r(r-1)/2} if there are \eqn{r} functions.}

\item{fun}{A function to aggregate results across pairs of functions, 
such as \code{\link[base]{mean}}, \code{\link{absmean}}, etc.}
}
\description{
Perform a Kruskal--Wallis rank sum test (ANOVA) for a summary statistic 
over pairs of functions using \code{\link[stats]{kruskal.test}}.
}
