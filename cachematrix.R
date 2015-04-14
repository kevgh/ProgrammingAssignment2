## makeMatrix is a function that caches an inverted
## matrix to hopefully reduce run time by using the
## cached value instead of recomputing it.
## We are using solve(X) to compute the inverse.

## A caching mechanism for the inverted form of a 
## matrix based on the makeVector example from class notes.
## The result is a list of functions to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverted matrix
##  4. get the inverted matrix

makeMatrix <- function(x = matrix()) {
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
    ## set 'm' to the inverse value
    setinverse <- function(inverse) m <<- inverse
    ## return the inverse value. It will be null if setinverse is never called,
    ## or if a new matrix was supplied via the set function
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Uses a cache to improve performance when the inverse of matrix x is needed
## multiple times.
##
## Returns a matrix that is the inverse of 'x'. x is an R 'list' created by
## the makeMatrix function above.
##   If the inverse is cached, return the matrix from the cache.
##   If it is not, compute the inverse, store it in the cache, and return
##     that result.

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    ## Check if the value is cached
    if(!is.null(m)) {
        ## Return the cached value.
        message("getting cached data")
        return(m)
    }
    ## Value isn't cached so get data, then compute, set and return the inverse
    data <- x$get()
    m <- solve(a = data, ...)
    x$setinverse(m)
    m
}
