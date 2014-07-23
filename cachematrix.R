makeCacheMatrix <- function(x = matrix()) {
    d <- NULL
    set <- function(y) {
        x <<- y
        d <<- NULL
    }
    get <- function() x
    setInverse <- function(i) d <<- i
    getInverse <- function() d
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
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
