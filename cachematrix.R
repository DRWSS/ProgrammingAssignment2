## This file contains two functions which create a special object
## that stores a matrix and caches its inverse

## The makeCacheMatrix function sets up a list of function 
## to set/get/update the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<- NULL
  }
  get <- function() x
  setinv <- function(inv) m<<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of the sdtored matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
