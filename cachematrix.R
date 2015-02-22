## The Following functions creates a special matrix object, caches its inverse, or computes the inverse if the
## cached value is invalid. 

## The makeCacheMatrix function creates a special matrix object that caches its inverse.
## The cached inverse in initially set to NULL and is set to NULL whenever the data of the matrix is changed.

makeCacheMatrix <- function(x = matrix()) {
	 inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,	
             getInverse = getInverse)


}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, then the cached inverse value won't be a NULL
##	and cacheSolve retrieves the inverse from the cache.
## Otherwise, the cacheSolve computes the cache inverse using the solve(x) function and caches the computed value.

cacheSolve <- function(x, ...) {
         inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
