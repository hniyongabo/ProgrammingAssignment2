## These two functions work together to cache the inverse of a matrix.
## Inverting a matrix is computationally expensive, so instead of recomputing
## it every time, we store ("cache") the inverse the first time it is calculated
## and return the cached version on subsequent calls.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions (set, get, setinverse, getinverse) that
## allow the matrix and its inverse to be stored and retrieved from within an
## enclosing environment using R's lexical scoping rules (the <<- operator).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" object returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), it retrieves the inverse from the cache instead of
## recomputing it, saving time on repeated calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}