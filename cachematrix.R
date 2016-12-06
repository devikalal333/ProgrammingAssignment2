## This set of functions computes the inverse of a matrix in a cost efficient way. 
## If the inverse of a matrix has been calculated previously, it is cached and 
## retrieved rather than recomputed.

## Assumption: Matrix is always invertible.

## makeCacheMatrix() takes in a matrix and returns a list of functions that do the following:
## set: sets the value of the matrix
## get: fetches the value of the matrix
## setinverse: sets the value of the inverse of the matrix
## getinverse: fetches the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(mat) i <<- mat
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function takes the output of the makeCacheMatrix (list of functions) and 
## returns the inverse of the matrix. If the inverse is cached, it doesn't recompute the inverse.
## If the matrix is new and a cached copy of inverse is not there, it calls the setinverse 
## function to compute the inverse of the matrix.

cacheSolve <- function(x, ...) {
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
