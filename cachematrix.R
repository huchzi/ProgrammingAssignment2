## Two functions are defined here:
## 1) makeCacheMatrix(): it returns a list of functions that can be used to cache a matrix
## in the environment of the makeCacheMatrix function, retrieve this matrix, 
## cache the inverse of the matrix, and retrieve the latter
## 2) cacheInverse(): a function to calculate, cache and retrieve the inverse in the
## in the makeCacheMatrix-environment

## Here, the function makeCacheMatrix() is defined (detail, see above)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Here, the function cacheInverse() is defined (detail, see above)

cacheInverse <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
