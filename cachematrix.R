# m <- matrix(c(3, 2, 1, 1), nrow = 2, ncol = 2, byrow = TRUE)
# m1 <- makeCacheMatrix(m)
# creates a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # cached inverse of matrix
  inv <- NULL
  
  # reset the object, if set-method is called
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return the matrix
  get <- function() x
  
  # set the inverse 
  setinv <- function(inv) inv <<- inv
  
  # get the inverse 
  getinv <- function() inv
  
  # List of methods:
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}

# Returns a matrix that is the inverse of 'x'
# x has to be an object of type "makeCacheMatrix"
# Returns: The inverse of the matrix
cacheSolve <- function(x, ...) {
  
  # check = cached inverse is available?
  # TRUE = Return the value and stops running the function
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # FALSE = code gets the matrix, computes invers, 
  # stores the result in x and returns inverse.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
