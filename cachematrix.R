## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # m will be inverse matrix
        m <- NULL
        
        # set the original matrix and (re)init its inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # return the original matrix
        get <- function() x
        
        # this is called during the first cacheSolve() call
        setInverse <- function(inverted) m <<- inverted
        
        # return the cached inverted matrix
        getInverse <- function() m
        
        # return the list of functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        # Get the inverted value of matrix x (stored in x).
        m <- x$getInverse()
        
        # If the inverted value was cached (not NULL) ...
        if(!is.null(m)) {
                message("Returning cached data...")
                # ... return the inverse matrix and exit the function.
                return(m)
        }
        
        # If the inverted value was not cached...
        data <- x$get()
        
        # ... calculate it...
        m <- solve(data, ...)
        
        # ... store it in given x...
        x$setInverse(m)
        # ... and return it as result of the function.
        m
}
