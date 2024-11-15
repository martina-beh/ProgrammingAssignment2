
## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the matrix
  get <- function() x
  # Set the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  # Get the inverse matrix
  getinverse <- function() inv
  # Return list of methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function to compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  # First try to get the cached inverse
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If not cached, get the matrix and calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}