
## We want to cache the inverse of matrices so that we can use the cached value when we need 
## the inverse of the same matrix.

## This function caches given matrix (so we can check that our matrix is the same matrix that its 
## inverse is cached), and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  set <- function(newMat){
    x <<- newMat
    cachedInverse <<- NULL
  }
  
  get <- function() newMat
  
  setInverse <- function(inv) cachedInverse <<- inv
  
  getInverse <- function() cachedInverse
  
  
  matrix(data = list(set = set, get = get, setInverse = setInverse, getInverse = getInverse), nrow=2,ncol=2)
  
}


## This function gets a matrix x and tries to check whether there is a cached version of
## this matrix or not.
##    if there is, it returns that cached inverse of the x
##    else it computes the inverse of the x and caches the inverse value of x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInverse()
  
  if(!is.null(xInverse)){
    message("getting cached data")
    return(xInverse)
  }
  
  data <- x$get()
  xInverse <- solve(a = data, ...)
  x$setInverse(xInverse)
  
  xInverse
}
