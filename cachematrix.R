
##Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse.
##"makeCacheMatrix" contains 4 functions: set, get, setinverse, getinverse.

##(1)get is a function that returns the vector x stored in the main function.
##(2)set is a function that changes the vector stored in the main function.
##(3,4)setinverse and getinverse are functions that store the value of the input in a variable m into the main function makeVector (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function “cacheSolve” computes the inverse of the special matrix returned by "makeCacheMatrix". 
##If the inverse has already been calculated, then the "cachesolve" should retrieve the inverse from the cache. 
##If the inverse has not been calculated, data gets the matrix stored with "makeCacheMatrix", m calculates the inverse, and x$inverse(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
