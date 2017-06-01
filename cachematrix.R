## This set of functions is intended to provide a means to
## calculate the inverse of a matrix from cache

##The makeCacheMatrix function is used to provide a list
## of actions for setting/getting the initial matrix
## and setting/getting the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the
## matrix created within the above function

cacheSolve <- function(x, ...) {
        m <-x$getinverse()
        if(!is.null(m)) {
          message("getting matrix inverse")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
