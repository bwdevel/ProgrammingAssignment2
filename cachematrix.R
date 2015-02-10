## a set of functions to set/get a matrix and it's inversion
## as well storing the solution in cache for performance

## makeCacheMatrix is a set of functions for set and get
## a matrix and it's inversion
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## a function to cache the inversion of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
