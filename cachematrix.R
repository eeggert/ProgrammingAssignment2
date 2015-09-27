## These functions aim at calculating the inverse of a matrix.
## Precondition is always that x has to be invertible.

## X - an invertible matrix, 
## the function returns a list of functions for use as input in cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  invert = NULL
  set = function(y) {
    x <<- y
    invert <<- NULL
  }
  get = function() x
  setinv = function(inverse) invert <<- inverse 
  getinv = function() invert
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Input: output from makeCacheMarix. Returns inverse of the original matrix.
## If inverse exists in the cache, then it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  invert = x$getinv()
  
  if (!is.null(invert)){
    message("Retrieving cached data")
    return(invert)
  }
  
  mat.data = x$get()
  invert = solve(mat.data, ...)
  
  x$setinv(invert)
  
  return(invert)
}
