# makeCacheMatrix and cacheSolve save computational time by caching
# the matrix inverse and returning the previously calculated value
# if previously calculated

# makeCacheMatrix declares a list with set, get, setinverse 
# and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve returns either the cached value of the matrix inverse
# or calculates the inverse if not previously calculated

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Retrieving cached data...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}