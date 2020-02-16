## cacheMAtrix.R
## A pair of functions that cache the inverse of a matrix.

## This first function creates a special "matrix" object that can cache its inverse.
## invoke by using specialMatrixObject <- makeCacheMatrix()
## Then set the matrix to be inverted by specialMatrixObject$set(mtx),
## where mtx is an invertible matrix e.g. mtx <- matrix(c(1,2,1,1),ncol=2)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This 2nd function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## invoke my cacheSolve(specialMatrixObject); the matrix should have been set before (see 1st function)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    
}
