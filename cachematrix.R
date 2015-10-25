## The makeCacheMatrix and cacheSolve functions are used together to store a matrix 
## inputted by the user, and then to solve for the inverse of the matrix and store the 
## computed inverse in memory. 

## The makeCacheMatrix function takes a supplied matrix as input and creates a list of
## 3 functions: get, setinv, and getinv, which will be used, respectively, to print 
## the matrix from memory, store the inverse, and print the inverse from memory.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## The cacheSolve function solves for the inverse of the matrix supplied to the makeCacheMatrix 
## function. First, it checks the 'getinv' variable defined above to determine if the computed 
## inverse is already stored in memory. If so, the inverse is printed from the cache. If the 
## inverse has not been computed, cacheSolve will calculate the inverse of the matrix and supply 
## the result to 'setinv', which stores the inverse in cache memory.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
