## This peer-functions calculate the matrix's inverse. 

## This function creates a matrix which set values of the matrix, get those values of the matrix,
## set value of the matrix's inverse, get value of the matrix's inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() {
    inverse <- solve(x)
    inverse
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inverse of the special matrix created 
## with the above function. It first checks to see if the inverse 
## has already been calculated and the matrix has not changed. 

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}




