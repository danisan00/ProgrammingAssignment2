## My solution to Programming Assignment 2

## Creates a special "cache matrix", which is really a list containing a
## function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix

makeCacheMatrix <- function(matrix = matrix) {
  
  inverse <- NULL

  set <- function(mat) {
    matrix <<- mat
    inverse <<- NULL
  }
  
  get <- function() {
    return(matrix)
  }

  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() {
    return(inverse)
  }

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculates the inverse matrix of the special "cache matrix" created with the
## above function. However, it first checks to see if the inverse matrix has
## already been calculated. If so, it gets the inverse matrix from the cache and
## skips the computation. Otherwise, it calculates the inverse matrix and sets
## the value in the cache via the setInverse function.

cacheSolve <- function(cacheMatrix, ...) {
  
  cachedInverse <- cacheMatrix$getInverse()
  
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  
  cachedInverse <- solve(cacheMatrix$get(), ...)
  cacheMatrix$setInverse(cachedInverse)
  
  return(cachedInverse)
}
