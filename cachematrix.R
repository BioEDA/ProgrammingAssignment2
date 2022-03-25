## There are 2 functions in this assignment. Function 1 is 'makeCacheMatrix' 
## and it creates a matrix object that caches its own inverse. Function 2 
## is 'cacheSolve' and it creates the inverse of the matrix made by function 
## 1. Should the inverse already have been computed, function 2 will retrieve 
## it from cache. 

## Write a short comment describing this function
## function 1 (makeCacheMatrix) creates a “matrix” m, in which we can set the 
## matrix elements, get these, set the elements of the matrix's inverse and get 
## these. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## function 2 (cacheSolve) creates the inverse of m created with function 1. 
## In doing so, it checks if the inverse has already been calculated. If yes, 
## it retreives the inverse from the cache; if not, it calculates it
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix_to_invert <- x$get()
  m <- solve(matrix_to_invert, ...)
  x$setinverse(m)
  m
}
