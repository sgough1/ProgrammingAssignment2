## Using the makeCacheMatrix and CacheSolve functions I have provided a way to cache the inverse of a matrix 
## and reduces the computing requirement to compute the inverse of a an unchanged matrix

## makeCacheMatrix creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function()x

        setinverse <- function(inverse)cache <<- inverse
        getinverse <- function()cache
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## CacheSolve computes the special matrix returned by makeCacheMatrix and if the inverse has already be calculated and the matrix has not changed then it should retrieve the result from the cache.

cacheSolve <- function(x, ...) {
        cache <- x$getinverse()
        if(!is.null(cache)) {
                message("Retriving cached data")
                return(cache)                
        }
        data <- x$get()
        cache <- solve(data, ...)
        x$setinverse(cache)
        cache     
}
