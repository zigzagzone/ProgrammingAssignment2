## This file consist of 2 functions
## 1. makeCacheMatrix - function to make a special matrix object
## 2. cacheSolve - function to cache a inverse of special martrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseData <- NULL
  set <- function(ma) {
    matrixData <<- ma
    inverseData <<- NULL
  }
  get <- function() matrixData
  setinverse <- function(inv) inverseData <<- inv
  getinverse <- function() inverseData
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseData <- x$getinverse()
  if(!is.null(inverseData)) {
    message("getting cached data")
    return(inverseData)
  }
  matrixData <- x$get()
  inv <- solve(matrixData, ...)
  x$setinverse(inv)
  inv
}
