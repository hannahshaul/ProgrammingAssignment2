## The purpose of the two functions are to cache the inverse of a matrix and retrieve it.

## The first function creates a matrix object, finds the inverse, then caches the inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        getMatrix <- function()
                x
        setInverse <- function(solve)
                m <<- solve
        getInverse <- function()
                m
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
        
}


## Per the assignment instructions, if the inverse has already been calculated and the matrix has not changed,
## the second function is supposed to retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}