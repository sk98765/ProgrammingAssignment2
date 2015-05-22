## This program contains 2 functions 
## These 2 functions will work together to calculate 
##  inverse matrix

## makeCacheMatrix use a square matrix as input and return 
##  a list of function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will use the function return by the makeCacheMatrix
##  to calculate the inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    message("new inverse matrix")
    return(inverse)
}
