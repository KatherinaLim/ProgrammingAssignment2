## This source file defines functions to create a cached matrix (makecacheMatrix) and calculate the inverse of this 
## cached matrix (cacheSolve)
## The calculation of the inverse of the matrix is fast after the first call of this function cacheSolve, because the 
## result of the first call will be cached and retrieved from cache after the first call instead of recalculating
## the inverse

## Creates a cached matrix
## Input: matrix
## Output: a list of 4 functions
##         - get: to retrieve the cached matrix
##         - set: to cache a matrix
##         - getinverse: to retrieve the cached inverse (NULL if the inverse of the current cached matrix was
##                       never calculated)
##         - setinverse: to cache the inverse matrix
## Note: the inverse that is cached does not necessarily have to be the inverse of the cached matrix if the inverse
## is not calculated with help of the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## define function to initialize a cached matrix with an empty cached inverse
  set <- function(y) {
    ## cache new matrix in x
    x <<- y
    ## cached inverse is set to NULL if this function is called, because then the inverse of the matrix is different
    i <<- NULL
  }
  
  ## define a function to retrieve the cached matrix
  get <- function() x
  
  ## define a function to cache the inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## define a function to retrieve the cached inverse
  getinverse <- function() i
  
  ## return the 4 defined functions as the result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function takes as input a makeCacheMatrix x and returns as output the inverse of x if x is invertible
## or an error if the matrix is not invertible. 
## If the cacheSolve function is called for the first time for the makeCacheMatrix x, then the inversion result is  
## calculated by the solve function and is also stored in cache.
## If the cacheSolve function has been called before for the same makeCacheMatrix x, then the inversion result is 
## read from cache instead of 
## Note: this function gives the same error message as the solve function when this function is run on a non
## invertible matrix. No code is added to ensure that the input is an invertible matrix.
cacheSolve <- function(x, ...) {
  
    ## Return a matrix that is the inverse of 'x'
    
    ## get the cached inverse
    i <- x$getinverse()
    if(is.null(i)) {
      ## there is no cached inverse, so it needs to be calculated and cached so it can be used without 
      ## recalculation next time
      i <- solve(x$get(), ...)
      x$setinverse(i)
    } else {
      ## there is a cached inverse, so the cached value can be returned instead of recalculating it
      message("getting cached data")
    }
    
    ## return the cached or calculated inverse
    i
}
