## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cacheInverse <- NULL
  
  set <- function(newMatrix){
    x <<- newMatrix
    cacheInverse <<- NULL
  }
  
  get <- function(){x}
  
  setInverse <- function(newInverse){
    cacheInverse <<- newInverse}
  
  getInverse <- function(){cacheInverse}
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheInverse <- x$getInverse
  
  if(!is.na(cacheInverse)){
    message("getting cached inverse!")
    return(cacheInverse)
  }
  matrix <- x$get
  cacheInverse <- solve(matrix)
  x$setInverse(cacheInverse)
  cacheInverse
}
