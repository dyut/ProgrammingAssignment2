## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. Here we create a special matrix
## which stores it's inverse. Inverse is computed only once for the very first time. Subsequently
## the cached value is return.


## This function creates a special "matrix" object that can cache its inverse
## Assumption is that 'x' is always invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(invX) inv <<- invX
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invX <- x$getInv()
    if (!is.null(invX)) {
        message("Getting cached Inverse")
        return(invX)
    }
    invX <- solve(x$get(), ...)
    x$setInv(invX)
    invX
}
