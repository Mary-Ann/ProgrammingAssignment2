## Functions to allow caching of matrix inversion
## - makeCacheMatrix to create a cache of a matrix and its inverse, and functions to control access to the cache
## - cacheSolve to read the inverse (from cache where possible)



## Takes a matrix argument x, creates a cache for the matrix and its inverse, 
## and returns a list of functions to get/set the matrix and its inverse.
## -- The set function will store the new matrix value, and clear the inverse from cache, 
## -- so the inverse will be recalculated, using the the new matrix value, when it is next read

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solved) inv <<- solved
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




## Takes a matrix cache as argument x, and returns its inverse
## - reads the inverse from cache
## - if the inverse is  not null returns it 
## - otherwise, the inverse is calculated, cached, and returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
