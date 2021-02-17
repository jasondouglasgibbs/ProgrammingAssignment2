## Programming Assignment 2 - Jason Gibbs##
## The functions cache the inverse of a matrix##

## The first function creates a special matrix object##

makeCacheMatrix <- function(x = matrix()) {
  ##Lines 8-13 are similar to the example provided in the assignment instructions.##
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##Follows the same construct as the example, except using function "inverse"##
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  ##List follows same construct as the example, just changed to inverses.##
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates and returns the inverse matrix of the supplied x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. Uses same construct as the example, only with inverses.##
  m <- x$getinverse()
  if (!is.null(m)) {
    message("Returning Inverse Matrix from Cache")
    return(m)
  }
  data <- x$get()
  ##Uses the solve function to return the inverse##
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
