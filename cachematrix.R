## The primary aim of the assignment is to write a pair 
## of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly 



## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) matinv <<- inverse
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  matinv <- x$getinv()
  ## If the inverse is already cached
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  ## Computing and returning the inverse of the matrix
  temp <- x$get()
  matinv <- solve(temp, ...)
  x$setinv(matinv)
  matinv
  
}
