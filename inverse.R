makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(x) m <<- solve(x)
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverseun(m)
  m
}

