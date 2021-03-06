## Demonstrate functionality of caching result of an expensive calculation (matrix inverse)
## Joe Knapp  jmknapp@gmail.com 1/22/2015

## Create a matrix with functions get(), set(), setinv() and getinv()
makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL  # inverse cache is initially null
  
  set <- function(y) {
    x <<- y         # set matrix to given one y
    invx <<- NULL   # reset inverse cache
  }
  
  get <- function() x   # return current matrix
  
  setinv <- function(inv) invx <<- inv  # set matrix inverse to given one inv
  getinv <- function() invx             # get cache matrix inverse
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Solve for inverse of x, use cached result if available, else call solve() and store
## result in cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  
  # is the cached inverse null?
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)  # got it, no need to recalculate, return cached inverse
  }
  
  # if we're here, then the cached inverse is null
  data <- x$get() # get matrix
  invx <- solve(data, ...)  # get its inverse
  x$setinv(invx)  # set the cached inverse
  invx  # and return it
}
