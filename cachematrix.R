## This file contains two functions to support a caching mechanism to avoid
## repeatedly calculating the inverse of a matrix. The first is makeCacheMatrix,
## which contains the cached data and provides a means to get and set both the
## original matrix and the cached inverted matrix. The second method is
## cacheSolve, which computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

## function makeCacheMatrix:
## A caching mechanism for the inverted form of a matrix based on the makeVector
## example from class notes. Creates a "special matrix" and returns a list of
## functions to:
##  1. set the matrix
##  2. get the matrix
##  3. set the inverted matrix
##  4. get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialize m (inverted matrix) to NULL.
    m <- NULL
    ## function to set the matrix to be inverted. When called, also resets the
    ## cached inverse matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the input matrix
    get <- function() x
    ## set 'm' to the supplied inverse value
    setinverse <- function(inverse) m <<- inverse
    ## return the inverse value. It will be null if setinverse is never called
    ## or if a new matrix was supplied via the set function
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function cacheSolve:
## Returns a matrix that is the inverse of the "matrix" 'x' created by
## createCacheMatrix.
##   If the inverse is cached, return it from the cache.
##   If it is not, compute the inverse, store it in the cache, and return
##     the result.

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    ## Check if the value is cached
    if(!is.null(m)) {
        ## Return the cached value.
        message("getting cached data")
        return(m)
    }
    ## Value isn't cached so get data, then compute, set (cache) and return the
    ## inverse
    data <- x$get()
    m <- solve(a = data, ...)
    x$setinverse(m)
    m
}
