## This script can cache inverse of matrices

## This function is like a class of cached matrices

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setInverse <- function(inv) m<<-inv
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse using "solve" or uses a cached matrix if available

cacheSolve <- function(x, ...) {
    m<-x$getInverse()
    if(!is.null(m)){
      print(" Already cached so using that")
      return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    return(m)
        ## Return a matrix that is the inverse of 'x'
}
