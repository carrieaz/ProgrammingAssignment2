## This program will cache the inverse of a SQUARE matrix. 
## Matrix inversion is usually very costly in computation. 
## Thus it is benefitial to cache the inverse of a matrix and retrieve it when it is needed
## calculate it if it is new matrix.
## Nov. 23 2014

## makeCacheMatrix will create a special matrix with the given matrix
## It will return a list of 4 functions that are associated with a special matrix 
makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinv <- function(inv) v <<- inv
        getinv <- function() v
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve will return a matrix that is the inverse of 'x'
## It either get it from the cache if it exits
## or calculate it if it is new
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data)
        x$setinv(v)
        v  
}
