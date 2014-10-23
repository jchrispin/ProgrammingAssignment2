## These functions are to cache the inverse of a matrix to avoid re-computation of the same matrix.

## makeCacheMatrix function will create a list of functions used for caching the inverse of a invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve relies on makeCacheMatrix to generate the inverse of a matrix directly from the cache if the matrix did not change or cache the new calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}