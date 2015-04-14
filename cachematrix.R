## This function creates a cached matrix using 
## its helper functions.
makeCacheMatrix <- function(x = matrix()) 
{
    matrixInvert <- NULL

    ## Sets matrix content
    setmatrix <- function(y) 
    {
        x <<- y
        matrixInvert <<- NULL
    }

    ## Returns matrix content
    getmatrix <- function() x

    ## Sets the inverted version of the given matrix
    ## This is the caching operation.
    setinvert <- function(invert) matrixInvert <<- invert

    ## Returns the inverted cached matrix
    getinvert <- function() matrixInvert

    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinvert = setinvert,
         getinvert = getinvert)
}

## This function returns the inverted version of a 
## given matrix.
##
## If the inversion of a matrix already exists, it
## is merely returned. Otherwise, it is calculated
## and returned.
cacheSolve <- function(x, ...) 
{
    ## Check if the inverted matrix is already cached
    invert <- x$getinvert()

    ## Found a cached invert, simply return it.
    if(!is.null(invert)) 
    {
        message("getting cached data")
        return(invert)
    }

    ## Cached invert not found
    ## Get matrix content
    data <- x$getmatrix()

    ## Compute the invert using the solve function
    invert <- solve(data, ...)

    ## Cache the invert for later use
    x$setinvert(invert)

    ## Return the computed invert
    invert
}
