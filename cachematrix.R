## Creating the inverse of matrix x. The inverted matrix is 
## retrieved from cache if it is stored in there, otherwise
## computed.

## makeCacheMatrix creates an environment for inversion of 
## matrix x. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inverse <<- solve
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

}


## cacheSolve returns the inverse for matrix x that it either 
## gets from cache if it is in there or computes for x caching  
## the result.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}
