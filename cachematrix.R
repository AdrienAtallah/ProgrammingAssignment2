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
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
## casheSolve computes the inverse of the matrix returned by
## makeCacheMatrix; if the inverse has already been calcuated
## the cacheSolve retrieves the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

## Test Run (after sourcing cachematrix.R):

#> a <- matrix(c(7, -2, 3, 5), 2, 2)
#> solve(a)
#[,1]        [,2]
#[1,] 0.12195122 -0.07317073
#[2,] 0.04878049  0.17073171

#> b <- makeCacheMatrix(a)
#> cacheSolve(b)
#[,1]        [,2]
#[1,] 0.12195122 -0.07317073
#[2,] 0.04878049  0.17073171

# It works!
