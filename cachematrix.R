## Put comments here that give an overall description of what your
## functions do
##The following functions take a matrix and cache and compute the inverse of that matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  b <- NULL
  set <- function(x) {
    x <<- y;
    b <<- NULL;
  }
  get <- function() return(x);
  setinv <- function(solve) b <<- solve;
  getinv <- function() b;
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  b <- x$getinv()
  if(!is.null(b)) {
    message("Getting cached data...")
    return(b)
  }
  data <- x$get()
  b <- solve(data, ...)
  x$setinv(b)
  return(b)
}
