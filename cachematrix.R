## The following two functions are used to cache the Inverse of a Matrix:
## Because Matrix inversion is usually a costly computation, there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.


## First function to create a special "matrix" object that can cache its inverse.
## This "matrix" is actually a list of four functions. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv<- function(solve) {inv <<- solve}
  getinv <- function() {inv}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Second function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat,...)
  x$setinv(inv)
  inv
}

