## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
# This function creates a special "matrix" object that can cache its inverse.
# Returns a list containing the matrix and its inverse in cache
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

    # set inverse matrix
    setInverseMatrix <- function(inverseMatrix) cacheInverse <<- inverseMatrix

    # get inverse matrix
    getInverseMatrix <- function() { cacheInverse }

    # return list for functions associated to matrix or inverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverseMatrix()
    # if it's null, then I need to calculate its inverse
    if(is.null(inverseMatrix)) {
        message("calculating cached data")
        # get matrix
        data <- x$get()
        # calculate its inverse
        inverseMatrix <- solve(data)
        x$setInverseMatrix(inverseMatrix)
    }
    inverseMatrix
}
