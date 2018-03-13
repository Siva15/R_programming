
# Week 3 assignment is about writing a R function to cache the output matrix inverse
# Matrix inverse takes longer time to compute as it involves heavy mathametical calculations
# Hence, it is important to cache the output and if the input doesn't change, the program should use the
# cache value rather than recomputing it.

# The function makeCacheMatrix creates a list containing the function set the value, get the value, 
# get the value of inverse and set the value of inverse


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    x <<- y 
    i <<- NULL  
  }
  
  get <- function() x
  
  setinv <- function(inv) i <<- inv
  
  getinv <- function() i
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
