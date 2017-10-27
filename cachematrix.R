##These functions serve to cache the inverse of a matrix in a special "matrix."

## This function creates a special "matrix" which contains a function that 1) sets the value of the matrix, 2) gets the value of the matrix, 3) sets the value of the inverse, and 4) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get<- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

##This function solves for the inverse of the matrix created by makeCacheMatrix above.
##If the inverse of the matrix has already been solved and the matrix does not change, then cacheSolve will retrieve the inverse from its cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
