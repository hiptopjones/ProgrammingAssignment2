## Matrix inversion is expensive.  Working together, these functions allow 
## clients to only compute the inverse of a specific matrix only once,
## caching the value for subsequent callers.

## This function creates a special matrix (really a list) that contains
## functions to get/set the matrix, as well as values to get/set the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of our special matrix created above.
## It will first check if the inverse has already been calculated.  If so,
## it will return the cached inverse.  if not, it will calculate the inverse,
## store it in the cache for the next caller, and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
