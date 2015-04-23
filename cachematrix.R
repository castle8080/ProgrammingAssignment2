##
## cachematrix.R
##
## Functions for caching the inverses of a matrix.
## 
## Use these functions if you need to repeatedly retrieve the inverse of a matrix.
## The inverse of the matrix will be stored alongside the matrix allowing you to
## retrieve the inverse without constantly recomputing it.
##
## Example:
##
## # Create a wrapper around a matrix with caching
## cm <- makeCacheMatrix(m)
##
## # Get the inverse
## inv <- cacheSolve(cm)
##
## # Get the underlying matrix from the cache wrapper.
## cm$get()
##
## # Reset the value of the matrix in the wrapper, which will invalidate the cache.
## cm$set(m)
##

## Create a wrapper around a matrix that supports caching of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    list(
        set = function(x_new) {
            x <<- x_new
            x_inv <<- NULL
        },
        get = function() {
            x
        },
        setinverse = function(x_inv_new) {
            x_inv <<- x_inv_new
        },
        getinverse = function() {
            x_inv
        }
    )
}


## Get the inverse from a wrapped matrix.  The inverse is cahced in the matrix wrapper.

cacheSolve <- function(x) {
    x_inv <- x$getinverse()

    if (is.null(x_inv)) {
        x_inv <- solve(x$get())
        x$setinverse(x_inv)
    }
    
    x_inv
}

## Tests the implementation of the caching functions.

testCache <- function() {
  
    genTestMatrix <- function(size) {
        matrix(sapply(1:(size*size), function(x) runif(1)), size, size)
    }
  
    m1 <- genTestMatrix(4)
    message("Generated matrix:")
    print(m1)
  
    # Test that the matrix was set in the wrapper.
    cm <- makeCacheMatrix(m1)
    if (any(cm$get() != m1)) {
        message("ERROR: matrix not set in wrapper.")
        return(FALSE)
    }
  
    if (!is.null(cm$getinverse())) {
        message("ERROR: the cache was already set.")
        return(FALSE)
    }
  
    inv <- cacheSolve(cm)
  
    message("Inverse:")
    print(inv)
  
    if (any(solve(m1) != inv)) {
        message("ERROR: The inverse was not correct.")
        return(FALSE)
    }
  
    if (is.null(cm$getinverse)) {
        message("ERROR: The inverse was not cached.")  
        return(FALSE)
    }
  
    # Check that the 2nd call still works.
    if (any(solve(m1) != inv)) {
        message("ERROR: The inverse was not correct.")
        return(FALSE)
    }
  
    # Hack the cache to make sure that the computation is not always being redone.
    cm$setinverse(-1)
    if (cacheSolve(cm) != -1) {
        message("ERROR: the cache was not used.")
        return(FALSE)
    }
  
    return(TRUE)
}