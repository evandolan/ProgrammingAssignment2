## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    
    # Set value of matrix
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    
    # Get value of matrix
    get <- function() x
    
    # Set value of inverse
    set_inverse <- function(inv) mat <<- inv
    
    # Get value of inverse
    get_inverse <- function() mat
    
    # Return list of functions
    list(set = set, get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Get cached value
    mat <- x$get_inverse()
    
    # Check if chaced value is not null
    if(!is.null(mat)) {
        message("getting cached data")
        return (mat)
    }
    
    # Caluclate inverse since it's not cached already
    data <- x$get()
    mat <- solve(data, ...)
    
    # Cache the new matrix
    x$set_inverse(mat)
    
    # Return the new matrix
    mat
}
