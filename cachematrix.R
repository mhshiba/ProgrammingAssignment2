## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse.
# Return a list containing functions to access matrix or its inverse (from
# cache, once its calculated)
makeCacheMatrix <- function(x = matrix()) {
    # initialize cache as NULL
    cacheInverse <- NULL

    # set matrix
    set <- function(x) {
        x <<- x
        # as it's a new matrix, it will be necessary to calculate its inverse
        cacheInverse <<- NULL
    }

    # get matrix
    get <- function() x

    # set inverse matrix in cache
    setInverseMatrix <- function(inverseMatrix) cacheInverse <<- inverseMatrix

    # get inverse matrix from cache
    getInverseMatrix <- function() { cacheInverse }

    # return list for functions associated to matrix or inverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
# Get inverse matrix.
# If it's already on cache, gets from cache. Otherwise, calculate it and store
# it in cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Try to get inverse from cache
    inverseMatrix <- x$getInverseMatrix()
    # if it's null, then it's the first time since
    # the matrix has been set. So I need to calculate its inverse
    if(is.null(inverseMatrix)) {
        message("calculating inverse matrix and caching it")
        # get matrix
        data <- x$get()
        # calculate its inverse using sol
        inverseMatrix <- solve(data)
        # save it in cache
        x$setInverseMatrix(inverseMatrix)
    }
    # Return inverse matrix, that was already on cache or has been calculated
    # right now and saved in cache to be used in next call.
    inverseMatrix
}
