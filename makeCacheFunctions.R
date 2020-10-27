# Assignment: Caching the Inverse of a Matrix

##  1.- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  2.- cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  setInverse  <- function(inverse) inv <<- inverse
  getInverse  <- function() inv
  list(set = set, get = get,
       setInverse  = setInverse,
       getInverse  = getInverse)
}
