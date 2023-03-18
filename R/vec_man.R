#' vec_subset
#'
#' Subsets the vector on given logical condition and returns the subset.
#' @param x A numeric vector.
#' @param condition any logical condition.
#' @export
#' @example
#'  /dontrun vec_subset(c(1,2,3,4,5),c(1,2,3,4,5) %% 2 == 0)
vec_subset <- function(x, condition) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  x[condition]
}

#' vec_split
#'
#' Splits vector into 2 lists with even and odd values separated.
#' @param x A numeric vector.
#' @export
#' @example
#'  /dontrun vec_subset(c(1,2,3,4,5,6))
vec_split <- function(x) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  even <- x[x %% 2 == 0]
  odd <- x[x %% 2 == 1]
  list(even = even, odd = odd)
}


#' vec_concat
#'
#' Concats multiple vectors in one vector.
#' @param ... arguments to be combined.
#' @export
#' @example
#'  /dontrun vec_subset(c(1,2,3),c(5,6,7),c(9,10))
vec_concat <- function(...) {

  c(...)
}




#' vec_sample
#'
#' Samples a specified number of elements from a vector.
#' @param x A numeric vector.
#' @param size Number of elements to  be returned as sample
#' @param replace a element will repeat in the sample, by default it will be FALSE.
#' @export
#' @example
#'  /dontrun vec_subset(c(1,2,3),c(5,6,7),c(9,10))
vec_sample <- function(x, size, replace = FALSE) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  sample(x, size, replace = replace)
}
