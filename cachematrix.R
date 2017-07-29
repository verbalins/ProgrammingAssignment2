## These functions will create the inverse from a provided matrix and cache it in makeCacheMatrix
## and return the cached answer from cacheSolve. If there aren't alreadyt a cached version,
## it will inverse the new one.

## This function will create a matrix and cache the inverse of it with solve().
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(x) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return a cached inverse matrix if available.
## Else it will inverse it and return the new inverse.
## x in this case is an makeCacheMatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
