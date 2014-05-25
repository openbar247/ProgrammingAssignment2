## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly (there are also alternatives
## to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    isInverseCached <- function() !is.null(inverse)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse,
         isInverseCached = isInverseCached)    
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(x$isInverseCached()) {
        message("getting cached data")
        return(x$getInverse())
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
