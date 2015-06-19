## Author: Santi on April 18th 2015
## Version 1.0
## Last Update: 2015-06-18 10:18PM

## This functions acts as a container for a list of GET/SET functions
## It expects a Matrix as Input and returns a List of operators
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y ## assign to parent environment
        inv <<- NULL ## reset
    }
    get <- function() x
    setinv <- function(minv) inv <<- minv
    getinv <- function() inv ## it doesn't actually compute the inverse
    
    ## the following is a Lis of functions that acts a the operators
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the Inverse of a given Matrix for the first time
## If it was previously invoked it will returns the value from the cache 
## instead of computing it, unless the value of the original matrix has changed
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    ##if the variable is already assigned then return it's value
    if (!is.null(inv)) {
        message("getting data from cache")
        return(inv)
    }
    
    ## Otherwise compute, set and return the inverse
    original <- x$get()
    inv <- solve(original)
    x$setinv(inv)
    inv
}
