# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.
# Assumption: the matrix supplied is always invertible.

#' This function creates a special "matrix" object that can cache its inverse.
#' @param x A matrix to be inverted
#' @return A list of functions to work with the given matrix: get, set, getInv, setInv
#' \describe {
#'  \item{get}{Get the original matrix \code{x}}
#'  \item{get}{Set the original matrix \code{x} values}
#'  \item{getInv}{Get the inverted matrix for matrix \code{x}}
#'  \item{setInv}{Set the inverted matrix values for matrix \code{x}}
#' }
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getInv <- function() inv
  setInv <- function(inverted) inv <<- inverted
  list(get = get, set = set, getInv = getInv, setInv = setInv)
}


#' Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#' If the inverse has already been calculated (and the matrix has not changed),
#' then the cachesolve should retrieve the inverse from the cache.
#' @param x A matrix to be inverted
#' @param ... optional parameters for the solve() function
#' @return An inveresed matrix \code{x}. The value is calculated if the matrix \code{x} does not contain the cached inversed matrix vlaues. Otherwise, the cache value is returned.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
