## makeCacheMatrix and cacheSolve are functions that get a matrix,
## calculate its inverse and cache it, avoiding the need of future recalculation.

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## cacheSolve function returns the inverse matrix computed from x.
## If the inverse has already been calculated, cachesolve will return
## the inverse already present in the cache, without recalculating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting data from cache")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
        
}
