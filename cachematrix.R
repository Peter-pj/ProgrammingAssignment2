## This is an implementation of a matrix wrapper capable of caching the inverse of the matrix.
## If the matrix is large and the inverse is needed repeatedly, the computational savings can be significant.
## The cache is invalidated if the contained matrix is replaced.

## The function makeCacheMatrix creates a matrix wrapper, which is really a list containing functions to:

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) x_inv <<- inv
  get_inv <- function() x_inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## The function cacheSolve calculates the inverse of a matrix wrapped in the caching wrapper and
## stores the inverse in the cache before it is returned. If the inverse is already in the cache,
## inverse is returned immediately.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$get_inv()
  if(!is.null(x_inv)) {
    message("getting cached inverse")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$set_inv(x_inv)
  x_inv
}
