
## Creates a cached inverse of the matrix, x, 
## and functions required to retrieved the cached matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #sets the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #gets the matrix
  get <- function() {
    x
  }
  #caches the inverse of the matrix
  setinv <- function(solve) {
    inv <<- solve
  }
  #gets the cached inverse of the matrix
  getinv <- function() {
    inv
  }
  #functions are returned so they can be called
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns a matrix that is the inverse of 'x'. 
## If the result is already cached it takes it from there.
## Otherwise it computes it. 
cacheSolve <- function(x, ...) {
  #retrives from cachematrix
  inv <- x$getinv()
  #Checks if the result was there and returns the result if available
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #Gets the matrix
  data <- x$get()
  #compute the inverse
  inv <- solve(data)
  #cache the inverse
  x$setinv(inv)
  #returns the inverse
  inv
}



