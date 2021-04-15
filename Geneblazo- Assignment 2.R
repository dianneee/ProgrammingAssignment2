makeCacheMatrix <- function(x = matrix()) {
  Dell <- NULL
  set <- function(y) {
    x <<- y
    Dell <<- NULL
  }
  get <- function() x
  setmean <- function(mean) Dell <<- inverse
  getmean <- function() Dell
  list (set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cacheSolve <- function(x, ...) {
  Dell <- x$getinverse()
  if (!is.null(Dell)) {
    message("getting cached data")
    return(Dell)
  }
  data <- x$get()
  Dell <- solve(data,...)
  x$setinverse(Dell)
  Dell
}

