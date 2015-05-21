## implements a caching for the inversion square matrices.  a matrix is
## wrapped by a list of functions to access the matrix and its inverse with
## the makeCacheMatrix() function.  the cacheSolve() function operated on the
## wrapper object to return the matrix inverse while checking to see if a
## cached value is present.

## makeCacheMatrix: create a special matrix object that can cache its
## inverse.  object (list) has members "set", "get", "setinverse", and
## "getinverse"
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function() {
        inv
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: compute the inverse of a special matrix.  the "special
## matrix" must have been created with the makeCacheMatrix() function and
## have methods "getinverse", "get", and "setinverse"
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

