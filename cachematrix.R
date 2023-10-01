## The makeCacheMatrix function creates a special "matrix" object which can
## cache its inverse, while the cacheSolve function computes the inverse of the 
## special "matrix" object created by makeCacheMatrix

## This function creates a special "matrix" object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object created 
## by makeCacheMatrix. It first checks if the inverse of the matrix has already 
## been computed and cached. If so, it retrieves and returns the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
