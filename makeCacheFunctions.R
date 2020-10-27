# Assignment: Caching the Inverse of a Matrix

##  REQUIREMENTS for the Assignment:
##  1.- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  2.- cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()){        # creating function makeCacheMatrix, X is the input Matrix
  inv <- NULL                                     # initializing "inv" variable to NULL.
  set <- function(y) {                            # assigning the function to "set" variable.
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x                              # assigning function to "get" variable.
  setInverse  <- function(inverse) inv <<- inverse
  getInverse  <- function() inv
  list(set = set, get = get,
       setInverse  = setInverse,
       getInverse  = getInverse)
}


cacheSolve <- function(x, ...){                   # # creating function cacheSolve, X is the input Matrix
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setInverse(inv)
  inv
}