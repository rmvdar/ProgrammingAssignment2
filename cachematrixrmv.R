## R Programming Assignment 2: Lexical Scoping; Caching the Inverse of a Matrix

## This R function is able to cache potentially time-consuming computations

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
  }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## The below function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above 

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
  }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
#Solve/check functions by running code with a matrix
