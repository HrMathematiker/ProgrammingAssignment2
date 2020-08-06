## Returns a list of functions to set and to get matrix and its inversion
## NOTE: inversion == NULL as default
## Example: 
##  m1 <- matrix(1:4, nrow = 2, ncol = 2)
##  m2 <- makeCacheMatrix(m1)
##  m2$get()
##  >>      [,1] [,2]
##  >> [1,]    1    3
##  >> [2,]    2    4
makeCacheMatrix <- function(x = matrix()) {
  inv.x <- NULL
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inv.x <<- inv
  getInv <- function() inv.x
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}

## Returns an inversion of given matrix
## NOTE: matrix should be represented as result of makeCacheMatrix
## Example: 
##  m1 <- matrix(1:4, nrow = 2, ncol = 2)
##  m2 <- makeCacheMatrix(m1)
##  cacheSolve(m2)
##  >>      [,1] [,2]
##  >> [1,]   -2  1.5
##  >> [2,]    1 -0.5
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting caching data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}