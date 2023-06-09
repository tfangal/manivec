% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vec_arith.R
\name{replace_na}
\alias{replace_na}
\title{replace_na}
\usage{
replace_na(x, replace_with)
}
\arguments{
\item{x}{A numeric vector.}

\item{replace_with}{A single value or a vector.}
}
\description{
Replaces NA values with user defined values in a vector.
}
\note{
replace_with vector's elements must be equal to the NA values, or you can also pass a single value.
#' @example
 /dontrun replace_na(c(5,4,NA,2,1),3)
}
