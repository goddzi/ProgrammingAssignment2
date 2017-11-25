## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    in <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) in <<- inverse
  getinv <- function() in
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  in <- x$getinverse()
  if(!is.null(in)) {
    message("getting cached data")
    return(in)
  }
  data <- x$get()
  in <- inverse(data, ...)
  x$setinverse(in)
  in
}
