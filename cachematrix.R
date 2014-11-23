## Computs and chaches the inverse of a square matrix to avoid repeatedly computations
## of costly calculations


## caches the result of matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set  <- function(y) {
        x <<- y
        inverse <<- NULL 
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

## Computs the inverse of a square matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}