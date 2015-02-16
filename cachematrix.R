## 


## This function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize a placeholder to store the inverted matrix
  m <- NULL
  
  ## Sets the matrix x to a new matrix y; resets the inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Returns matrix x
  get <- function() x
  
  ## Sets the inverse m using solve()
  setinv <- function(solve) m <<- solve
  
  ## Returns the inverse m
  getinv <- function() m
  
  ## Returns a list containing the four functions defined above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Gets the inverse (m) of the matrix defined in function x
  m <- x$getinv()
  
  ## If that inverse already exists, print line indicating you're grabbing it from the cache. Return the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If m does not exist, run the code below:
  
  ## Get the matrix defined in function x
  data <- x$get()
  
  ## Get the inverse of the matrix using solve()
  m <- solve(data, ...)
  
  ## Store the inverse for the matrix defined in function f
  x$setinv(m)
  
  ## Return the inverse, m
  m
}
