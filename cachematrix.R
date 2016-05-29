## Performing solve() function with caching.

## Describes the object containing methods for operating with the matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) cache <<- solve
    getsolve <- function() cache
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Calculates the inverse of a matrix or returns the cached one.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
