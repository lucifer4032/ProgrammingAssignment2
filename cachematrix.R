## Functions to calculate inverse of a matrix and cache the result (or retrieve the inverse of a matrix if already calculated)
## Usage: Pass the result of a makeCacheMatrix call to cacheSolve 



#' Util function that set or get the matrix and the inverse in an environment
#' @param x an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



#' Check whether the inverse of the matrix is already calculated
#' if yes, then the inverse is retrieved from the cache (i is equal to null)
#' if no, then compute and cache the inverse of a matrix
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) { 
        message("Getting cached inverse of a matrix!")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}