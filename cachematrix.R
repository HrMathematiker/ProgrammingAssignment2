## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv.x <- NULL
  set <- function(y) {
    x <<- y
    inv.x <- NULL
  }
  get <- function() x
  setinv <- function(inv) inv.x <<- inv
  getinv <- function() inv.x
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting caching data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}