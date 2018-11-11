## These two functions would cache the inverse of a matrix first,
## then return the inverse from the cache.

## This function can set and get the value of a matrix, and cache
## the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invr <<- inverse
        getinverse <- function() invr
        list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)

}


## This function would return the cached inverse.
## If the inverse has not been calculated, the function would compute the inverse and return it.

cacheSolve <- function(x, ...) {
        invr <<- x$getinverse()
        if(!is.null(invr)) {
                message("Getting Cached Data:")
                return(invr)
        }
        matrx <- x$get()
        invr <- solve(matrx, ...)
        x$setinverse(invr)
        invr
}
