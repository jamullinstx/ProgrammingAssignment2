## Programming Assignment 2 for Coursera R-Programming class

# The makeCacheMatrix function (really a list of functions) provides 4 methods
# to set & get a matrix, and to set & get the inverse of the matrix. It caches
# the matrix and its inverse to save computations in the case that a program
# requests the value of the inverse and the matrix hasn't changed

# The second function computes the inverse and returns it. The cached value
# gets returned if the data haven't changed.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(matrixinverse) minv <<- matrixinverse
    getinverse <- function() minv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    
    # Grab the value of minv
    minv <- x$getinverse()
    # Test if it is NULL, indicating that a set operation has been done since
    # the last time it was calculated. If it is !NULL, then just return the
    # cached value.
    if(!is.null(minv)) {
        # The matrix hasn't changed, so just return the cached value
        message("retrieving cached data")
        return(minv)
    }
    # Since the matrix has evidently changed, let's grab the latest
    datamatrix <- x$get()
    # Compute its inverse using solve()
    minv <- solve(datamatrix)
    # Set the value of the inverse in the cache data structure
    x$setinverse(minv)
    minv
}
