## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly
## The 2 functions in this file are use to cache the inverse of the matrix

## makeCacheMatrix creates a list containing a function to
# set the value of the matrix, get the value of the matrix
# set the value of inverse of the matrix, get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## The following function returns the inverse of the matrix. If the inverse has been calculated already
#  it returns the cached value, otherwise it calculates the inverse using the solve function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting Cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
