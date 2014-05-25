## These methods are used to compute inverse of a matrix and caches 
## the result in global environment. If the matrix is previously computed
## it will get and return the inverse from the chache.

## Creates a special matrix that can catch its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of a matrix, store it in cache and return the result. If the matrix already been 
## calculated it will get the result from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inverse)
  }
  iMat <- x$get()
  inv <- solve(iMat)
  x$setinverse(inv)
  inv
}
