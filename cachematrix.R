## Creates the cache of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## Initialize the inverse matrix
  j <- NULL
  
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  get <- function()x
  
  setInverse <- function(inverse) j <<- inverse
  
  getInverse <- function() j 
  
  ## List of the methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Checks if the value is stored in the cache and recover it or save it in 
## the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("Getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

