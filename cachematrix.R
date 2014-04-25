## Programming Assignment 2
## Peer Assessment for Coursera (rprog-002)

## makeCacheMatrix: this function creates a spectial type of matrix that can
## cache it's inverse. It has it's own set and get functions which helps
## caching values into the environment it is called (in our case "cacheSolve")

makeCacheMatrix <- function(x = matrix()) {     # Use matrix object instead of vector
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()         # We query for the m value (in the first run it will return NULL)
    if(!is.null(m)) {           # If the m value is not NULL (i.e. it's in our cache);
        message("Got result from cache.")
        return(m)               # Then we return it's value
    }                           # At this point our function returns if our value is cached, otherwise:
    data <- x$get()             # We assign the matrix to data variable using the get method
    m <- solve(data, ...)       # And run the actual calculation using the solve method on 'data' matrix
    x$setInverse(m)             # Then we cache the value using the set method so we don't calculate it again
    m                           # We return the result
}
