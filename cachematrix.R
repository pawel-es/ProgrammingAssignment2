## A pair of functions that cache the inverse of a matrix. 
## solve(X) function is used to compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Variable where an inverse of the matrix will be stored
    invMatrix <- NULL
    
    # Function to set value of the matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    # Function to get value of the matrix
    get <- function() x
    
    # Function to set (cache) value of the inverse of the matrix
    setSolve <- function(solve) invMatrix <<- solve
    
    # Function to get value of the (cached) inverse of a matrix
    getSolve <- function() invMatrix
    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## function. If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve will retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Check if the inverse of the special "matrix" has been already calculated
    # and stored in invMatrix variable. If TRUE then return cached data.
    invMatrix <- x$getSolve()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    
    # If this object does not have inverse matrix cached - calculate it, store it 
    # and return.
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setSolve(invMatrix)
    invMatrix    
}
