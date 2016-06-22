## These Functions create an Inverse of a Matrix and store that information
## in a Cache. If we need to find the inverse of the same matrix then it 
## can be retrived from the cacheSolve Function

## This Fucntion creates an Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInvr <- function(inverse) invr <<- inverse
  getInvr <- function() invr
  list(set = set, 
       get = get,
       setInvr = setInvr,
       getInvr = getInvr)
}

## This Fucntion checks whether the inverse of a Matrix is already in the cache
## If it is in the cache, it retreives the cache data. Else, it creates the 
## Inverse of the Matrix and stores it in Cache.

cacheSolve <- function(x, ...) {
  invr <- x$getInvr()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  mtrx <- x$get()
  invr <- solve(mtrx, ...)
  x$setInvr(invr)
  invr
}
