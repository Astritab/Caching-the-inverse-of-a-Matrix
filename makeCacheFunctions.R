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

## For the second function, we take the other matrix.
## IF the Inverse Matrix exists, then it was already calculated and stored before. If not, it saves it in cache.

cacheSolve <- function(x, ...){                   # creating function cacheSolve, X is the input Matrix
  
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

# Testing both functions and how they are working with directo and inverse Matrix, using cache options.
# > myTest_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# > myTest_Matrix$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(myTest_Matrix)         First time, the Inverse matrix is not in cache. It's computed and stored in cache
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(myTest_Matrix)         Second time (and so on), the Inverse Matrix is in Cache. It's not computed, is retrieved from cache.
# getting cached result
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
