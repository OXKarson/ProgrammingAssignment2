## Cached inverse matrix

## Creates custom matrix object that supports a cached inverse operation
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculate and save inverse if cache is empty. Return inverse of input
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    i
  } else {
    x$setinverse(solve(x$get()))
    x$getinverse()
  }
}
