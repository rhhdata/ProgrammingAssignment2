## Put comments here that give an overall description of what your
## functions do
## solution of Assignment 2: Caching the Inverse of a matrix

## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## computes the inverse of the matrix and caches it
cacheSolve <- function(x) {
    inv <- x$getInverse()
    if ( is.null(inv)) {
        message("computing inverse")
        inv <- solve( x$get() )
        x$setInverse(inv)
    } else {
        message ("getting cached inverse")
    }
    
    ## return the inverse matrix
    inv
}
