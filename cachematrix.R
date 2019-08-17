## This file consist of 2 functions
## 1. makeCacheMatrix - function to make a special matrix object
## 2. cacheSolve - function to cache a inverse of special martrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## initial the data of 'inverseData' param
  inverseData <- NULL
  
  ## set function, to set matrix data and store in 'matrixData' param
  set <- function(ma) {
    matrixData <<- ma
    inverseData <<- NULL
  }
  
  ## get fuction, return 'matrixData'
  get <- function() matrixData
  
  ## setinverse function, set the inverse of matrix in 'inverseData' param
  setinverse <- function(inv) inverseData <<- inv
  
  ## getinverse function, return 'inverseData' that the inverse of 'matrixData'
  getinverse <- function() inverseData
  
  ## return a list contain set, get, setinverse, getinverse fuctions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## get the inverse data from special matrix object
  inverseData <- x$getinverse()
  
  ## check if it already cached
  if(!is.null(inverseData)) {
    
    ## found a cache!, return it
    message("getting cached data")
    return(inverseData)
  }
  
  ## cache not found, get matrixData from special matrix object
  matrixData <- x$get()
  
  ## call solve fuction to execute inverse of matrix
  inv <- solve(matrixData, ...)
  
  ## set inverse of matrix to special matrix object
  x$setinverse(inv)
  
  ## return inverse of matrix
  inv
}
