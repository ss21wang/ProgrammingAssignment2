## Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions: makeCacheMatrix and cacheSolve will compute the
## inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix object that can
## cache its inverse

##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of inverse of the matrix
##4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <-NULL
  set <-function(y){
    x <<- y
    inverse <<- NULL
  }
  get <-function()x
  setinv <-function(inverse)inverse <<- inverse
  getinv <-function() inverse
  list (set = set, get=get, setinv=setinv, getinv=getinv)
}


## The following function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  inverse <-x$getinv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <-solve(data)
  x$setinv(inverse)
  inverse
}


