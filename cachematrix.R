## The following functions can be used to calculate the inverse of
## a matrix a cache that value.

## This function returns a list of functions in order to
## set and get the value of the matrix
## set and get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(m) {
                x <<- m
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function receives the list returned by the above function
## as parameter and returns the matrix inverse. If the value of
## the inverse has already been calculated (and cached) and the
## matrix hasn't changed, the function will return that value
## otherwise it will perfom the calculation and cache the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
        } else {
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
        }
        inv
}
