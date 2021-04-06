##caches the inverse of a matrixas matrix inversion is a costly computation
##assumes the matrix is always reversable


## a special matrix object for caching the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse<- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##computes the inverse of the special matrix returned by makeCacheMatrix above
###If the inverse has already been calculated (and the matrix has not changed), 
###then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}

