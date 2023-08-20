## This peer-functions calculate the matrix's inverse. 

## This function creates a matrix which set values of the matrix, get those values of the matrix,
## set value of the matrix's inverse, get value of the matrix's inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inverse of the special matrix created 
## with the above function. It first checks to see if the inverse 
## has already been calculated and the matrix has not changed. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  m_inicial <- x$get
  if(!is.null(m) & !identical(m,m_inicial)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

primera_prueba <- makeCacheMatrix(x = matrix(data = rnorm(9), ncol = 3, nrow = 3))
cacheSolve(primera_prueba)
