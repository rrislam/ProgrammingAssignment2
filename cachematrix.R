## The following two functions can be used to cache the inverse of a matrix. 

## makeCacheMatrix creates a special kind of "matrix" which 
## is a actually a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


## cacheSolve returns the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been computed and the matrix hasn't changed, it retrieves 
## the inverse from the cache. Otherwise, it computes the inverse.

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
