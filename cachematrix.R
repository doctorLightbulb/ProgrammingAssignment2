## These functions cache and return the inverse of a matrix object.

## This function stores a matrix whose values can be retrieved.

makeCacheMatrix <- function(x = matrix()) {
    cacheMValues <- NULL
    set <- function(y) {
        x <<- y
        cacheMValues <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) cacheMValues <<- matrix
    getmatrix <- function() cacheMValues
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## And this function calculates the inverse of
## the matrix stored in makeCacheMatrix(). 
## NOTE: This function works only with an invertible matrix.

cacheSolve <- function(x, ...) {
    cacheMValues <- x$getmatrix()
    if(!is.null(cacheMValues)) {
        message("getting cached data")
        return(cacheMValues)
    }
    data <- x$get()
    cacheMValues <- solve(data, ...)
    x$setmatrix(cacheMValues)
    cacheMValues
}
