## These functions create a special object that stores a matrix and caches the inverse of it.


## This function creates a special "matrix" that is actually a list. The created list
## contains a function that can set or get the value of the matrix and set or get the value of its inverse.

makeCacheMatrix <- function(x = maxtrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function check to see if an inverse matrix has already been calcuated for the special matrix
## created by the previous function. If it has, then it gets the inverse from the cache. Otherwise it calculates it.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}