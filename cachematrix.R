## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## The first function, `makeCacheMatrix` creates a special list, which
## contains a function to: set the value of the matrix, 
## get the value of the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The following function calculates the inverse of the matrix stored
## by the above function. It first checks to see if the mean has already 
## been calculated. If so, it `get`s the mean from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}