## These functions cache the inverse of a matrix. If an inverse has been cached, the second function 
## doesn't need to recalculate it and can access it from the cache.

## This function creates a list that sets the value of a matrix,
## gets the value of a matrix, sets the value of the inverse,
## and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks to see if the inverse has been calculated. If so, it gets the inverse from
## the cache and skips the calculation; otherwise it calculates the inverse, stores it in the cache,
## and returns the inverse from the calculation.

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
