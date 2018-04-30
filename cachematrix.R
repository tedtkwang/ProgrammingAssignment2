## The makeCacheMatrix function creates a list containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) s <<- solve
    getInverse <- function() s
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of matrix provided by the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    s <- x$getInverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setInverse(s)
    s
}
