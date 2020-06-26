## Matrix inversion is a costly computationand there is benefit to caching the inverse of a matrix rather than compute it repeatedly
## That is what we will be doing with these functions

## this function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    q <- NULL
    set <- function(y){
      x <<- y
      q <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) {q <<- inverse}
    getInverse <- function() {q}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Cachesolve computes the inverse of the matrix returned by makecachematrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    q <- x$getInverse()
    if(!is.null(q)){
        message("getting cached data")
        return(q)
    }
    mat <- x$get()
    q <-solve(mat, ...)
    x$setInverse(q)
    q
}
