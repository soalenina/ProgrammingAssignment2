## The first of these two functions creates a matrix (assumed to be inversable), computes its inverse and caches it. The second returns the inverse of the first matrix, either by retrieving it from the cache, if it has already been calculated, or by computing it. 

## This function creates a special "matrix" object that cache its inverse, doing so by:
### (1) setting the value of the matrix
### (2) getting the value of the matrix
### (3) setting the value of the inverse of the matrix
### (4) getting the value of the pre-set inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns a matrix that is the inverse of the special "matrix" created by makeCachMatrix. To return such inverse matrix, it either
###(1) retrieves the pre-calculated inverse from the cache created by makeCacheMatrix, or
###(2) computes the inverse of the matrix.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
