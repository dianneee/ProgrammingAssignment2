makeCacheMatrix <- function(x = matrix()) {
  d <- NULL
  set <- function(y) {
    x <<- y
    d <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) d <<- inverse
  getinverse <- function() d
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  d <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(d)
  }
  data <- x$get()
  d <- solve(data,...)
  x$setinverse(d)
  d
}
