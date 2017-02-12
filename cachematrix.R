## CacheMatrix functions: create a special matrix capable of caching its inverse and calculate 
## that inverse. 
## 
## Note: Functions assume source matrix 'x' is always invertible. 

## makeCacheMatrix(x)
##
## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(invVal) inv <<- invVal
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve(x,...): 
## 
## Returns the inverse of special CacheMatrix 'x' from cache if possible, 
## otherwise it calculates the inverse and then and caches the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()

  # if previously calculated, returned cached value
  if(!is.null(inv)) {
    #message("getting cached data")
    return(inv)
  }

  # calculate inverse and cache the result
  data <- x$get()
  invVal <- solve(data, ...)
  x$setInverse(invVal)
  invVal
}
