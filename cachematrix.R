## Put comments here that give an overall description of what your
## functions do
# This set of functions allow caching the inverse of a matrix to avoid potentially time consuming operaitons
# to use:
# 1. creare a square matrix e.g. x <- matrix(rnorm(16), 4,4)
# 2. create a cached matrix by called makeCacheMatrix e.g. m <- makeCacheMatrix(x)
# 3. call cacheSolve(m) to get the cached matrix or a newly created one

# the functions will work only for inversible matrices

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## Write a short comment describing this function
# return the inverse of a square matrix from cache if matirx value unchanged
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      im <- x$getinverse()
      if(!is.null(im)){
          message("getting cached data")
          return(im)
      }
      mat <- x$get()
      im <- solve(mat, ...)
      x$setinverse(im)
      im
}
