## Adrien Atallah
## R Programming - Project 2

## makeCacheMatrix Creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
## casheSolve computes the inverse of the matrix returned by
## makeCacheMatrix; if the inverse has already been calcuated
## the cacheSolve retrieves the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
