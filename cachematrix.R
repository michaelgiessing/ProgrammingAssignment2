## Given an invertible matrix these functions
## create method to store and update the inverse
## of the matrix with it.

## Creates the environment and functions to get and 
## set the matrix and its inverse.
## Assumes that the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invrs) inv<<- invrs
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
  
}


## Calculates the inverse of the matrix and Caches it for use later.
## in the environment created above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  }
