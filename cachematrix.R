
## this script returns the inverse of a matrix. if the same matrix
## is passed in without any change, a cached copy of the result is returned

## this function creates a matrix object that is able to encapsulate the cached result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinver <- function(inver) m <<- inver
    getinver <- function() m
    list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## this function does the actual inverse calculation

cacheSolve <- function(x, ...) {
    m <- x$getinver()
    if(!is.null(m)) return (m)
    data <- x$get()
    m <- solve(data, ...)
    x$setinver(m)
    m
}
