## Put comments here that give an overall description of what your
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
## firstly, it sets the value of the matrix
## secondly, it gets the value of the matrix
## thirdly, it sets the value of the inverse
## lastly, gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) k <<- inverse
  getinv <- function() k
  list(set = set,
       get = get,
       setinverse = setinv,
       getinverse = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  l <- x$getinverse()
  if (!is.null(l)) {
    return(l)
  }
  data <- x$get()
  l <- solve(data, ...)
  x$setinverse(l)
  l
}
