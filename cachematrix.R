## The functions herein cache a variety of calculated
## matrix values

## Create the Cache Matrix as a list with all setters and getters
## enumerated. Usage:
##    x<- makeCacheMatrix
##    x$set(input matrix)
##    x$get()
## In order to set and get cached values, use their separate calling functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Calling function for cached matrix inverse. It will either return a
## cached version of the matrix inverse or solve, cache, and return the
## matrix inverse. Usage:
##    cacheSolve(previously set Cache Matrix)
## returns the matrix inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Ideas for improvement
## Instead of setting each cached value separately, use attribute-value pairs
## to set and get.
## Allow function to be cached as an argument instead of limiting to
## predefined functions
