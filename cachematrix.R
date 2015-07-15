## This script contains two functions that combine to cache the
## inverse value of a matrix

## makeCacheMatrix takes matrix x as input and augments it to
## create a special type of matrix. This returned object is in fact a list
## of functions to get and set the value of the matrix and get and set the 
## value of the inverse of x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve takes the "matrix" list object created by makeCacheMatrix as input x.
## Its purpose is to calculate the inverse of the matrix contained in x.
## It first checks if the inverse has already been calculated and if so
## returns that value.
## If not, it calculates the inverse, sets the "inv" variable in x and 
## then returns the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("Getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
