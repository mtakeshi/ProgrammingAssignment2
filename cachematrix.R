## These functions store a matrix and calculate its inverse matrix, keeping
##  the result stored in the cache, so it's not necessary to recalculate the
##  result.

## Function makeCacheMatrix() : this is a object that will keep the values of 
##  the matrix and its inverse. This object is controlled by its inner 
##  functions: set(), get(), setinverse(), getinverse(), reset()
## TIP: First assign this function to a variable then call the inner functions.
##  v <- makeCacheMatrix(); v$set(matrix(rnorm(1:9), nrow = 3)); v$get()
makeCacheMatrix <- function(mtx = matrix()) {
  ## when declaring the new matrix, reset the variables
  invs <- NULL
  ## define the inner function set() that stores the matrix and reset the 
  ##  inverse matrix
  set <- function(y) {
    mtx <<- y
    invs <<- NULL
  }
  ## define the inner function get() that returns the original matrix
  get <- function() mtx
  ## define the inner function setinverse() that stores the value of the 
  ##  inverse matrix
  setinverse <- function(inverse) invs <<- inverse
  ## define the inner function getinverse() that returns the inverse matrix
  getinverse <- function() invs
  ## this is a new function that resets the values stored in this object
  reset <- function() {
    mtx <<- NULL
    invs <<- NULL
  }
  ## list the inner functions of this function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse,
    reset = reset)
}

## Function cacheSolve() : this is a function which has the makeCacheMatrix object 
##  as first argument, and will calculate the inverse matrix stored in this
##  object. This function will also keep the calculated matrix in the object for
##  future reference.
## TIP: Inform the variable assigned for the first function in this function call
##  cacheSolve(v)
cacheSolve <- function(x, ...) {
  ## get the content of inverse matrix stored in the object
  invs <- x$getinverse()
  ## if the inverse matrix was already calculated then return it to the console
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  ## if there is no inverse matrix yet, get the original matrix...
  mtx <- x$get()
  ## and calculate it with the solve function, ...
  invs <- solve(mtx, ...)
  ## store the inverse matrix in the object...
  x$setinverse(invs)
  ## and return the result
  invs
}
