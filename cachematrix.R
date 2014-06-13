## makeCacheMatrix creates a special matrix type that can cache it's inverse to improve performance
## cacheSolve returns the inverse of a matrix created with makeCacheMatrix using cached value if available

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {

  inv <- NULL
  
  set <- function(n){
    m <<- n
    inv <<- NULL
  }
  
  get <- function() m
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'

  inv <- m$getInverse()
  
  if (!is.null(inv)){                 ##inverse is cached, use cached value
      message("Using cached inverse")
      return(inv)
  }
  
  n <- m$get()
  inv <- solve(n)    ## Compute inverse
  m$setInverse(inv)  ## Store inverse in cache
  
  inv
  
}
