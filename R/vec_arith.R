#' vec_min
#'
#' Returns minimum value from a vector.
#' @param x A numeric vector.
#' @export
#' @example
#'  /dontrun vec_min(c(1,2,3,4,5))
vec_min <- function(x) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  min(x)
}

#' vec_max
#'
#' Returns maximum value from a vector.
#' @param x A numeric vector.
#' @example
#'  /dontrun vec_max(c(1,2,3,4,5))
#' @export
vec_max <- function(x) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  max(x)
}

#' vec_sort
#'
#' Sorts a vector in ascending order.
#' @param x A numeric vector.
#' @param decreasing if TRUE then the vector will be in descending order.
#' #' @example
#'  /dontrun vec_sort(c(3,1,4,5,2))
#' @export
vec_sort <- function(x, decreasing = FALSE) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  sort(x)
}

#' vec_rev
#'
#' Reverses the passed vector.
#' @param x A numeric vector.
#' #' @example
#'  /dontrun vec_rev(c(5,4,3,2,1))
#' @export
vec_rev <- function(x) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  rev(x)
}


#' vec_uniq
#'
#' Removes all the duplicate values in a vector.
#' @param x A numeric vector.
#' #' @example
#'  /dontrun vec_uniq(c(5,5,4,3,2,2,1))
#' @export
vec_uniq <- function(x) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  unique(x)
}


#' replace_na
#'
#' Replaces NA values with user defined values in a vector.
#' @param x A numeric vector.
#' @param replace_with A single value or a vector.
#' @note replace_with vector's elements must be equal to the NA values, or you can also pass a single value.
#' #' @example
#'  /dontrun replace_na(c(5,4,NA,2,1),3)
#' @export
replace_na <- function(x, replace_with) {
  if(!is.vector(x)){
    stop("Invalid input! Function only accepts vectors.")
  }
  x[is.na(x)] <- replace_with
  x
}


