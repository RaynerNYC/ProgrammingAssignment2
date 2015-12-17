## Resubmission of Coursera Module 2 : R Programming Assignment 2:
## This function is use to create a special matrix that is a list containing function to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  ## 'x' is a matrix that's assumed to be always invertible.
  ## Return the inverse of matrix x.
  
  m <- NULL
  
  ## set x
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  
  ## get x
  get <- function() {
          x
  }
  
  ## cache(set) the inverse Matrix
  setMatrix <- function(invMatrix){
          m <<- invMatrix
  }
  
  ## get the cached inverse Matrix
  getMatrix <- function(){
          m
  }
  
  ## return a list
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  
  ## If the inverse matrix existed, will get from cache object
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## Create the inverse matrix 
  matrix <- x$get()
  m<-solve(matrix, ...)
  x$setMatrix(m)
  
  ## return the inverse matrix
  m
  
}
