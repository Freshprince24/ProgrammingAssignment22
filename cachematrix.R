## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix will make a matrix object that put its inverse in a cache
## set will store the latest matrix and remove any stored inverse
## get will retreive the matrix
## setinverse will calculate and set the inverse of the matrix
## get inverse will retreive a stored matrix if there's any



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 cache <- NULL
    set <- function(matrixInput) {
        x <<- matrixInput
        cache <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) {
        cache <<- inverse
    }
    
    getInverse <- function() cache
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve is a function to get the inverse of a matrix which will be
## made by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # checks for a cached index
    cachedInverse <- x$getInverse()
    if (!is.null(cachedInverse)) {
        message("getting cached data")
        return(cachedInverse)
    }
    
    # calculate the inverse using solve()
    matrixInput <- x$get()
    inverse <- solve(matrixInput, ...)
    
    # store the inverse in the cache
    x$setInverse(inverse)
    
    inverse
}
