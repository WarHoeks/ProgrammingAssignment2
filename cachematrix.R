## This script has two functions. The first function, makeCacheMatrix, creates a list of functions to both 
## set and get (if previously cached) the matrices and their inverse. The second funcion, cacheSolve is the
## function actually calculates the inverse of a matrix and stores it in the cache (retrievable with x$get())
## for convenience

## Write a short comment describing this function

## This function defines the accessors and mutators for the matrix value and its inverse
makeCacheMatrix <- function(x = matrix()) {
        # Create empty inv object
        inv <- NULL
        # Define the set function: set x to the new value y, and clean inv
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Define get function: return x
        get <- function() x
        # Define set_inverse function: give inv the value of inverse (passed argument from cacheSolve)
        set_inverse <- function(inverse) inv <<- inverse
        # Define get_inverse function: return inv
        get_inverse <- function() inv
        # Pass all functions in a named list for easy calling: object$get()
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This function is the one creating the inverse of the matrix, stores it in the inv object of the
## makeCacheMatrix function if it is a new calculation, and retrieves it from the get_inverse function
## if already cached

cacheSolve <- function(x, ...) {
        ## Retrieve the cached version (if there is none, retrieves NULL)
        inv <- x$get_inverse()
        # If not NULL, return the cached inverse
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # If no cached inverse matrix is stored, get the matrix
        data <- x$get()
        # inverse it
        inv <- solve(data)
        # And store it in the cache
        x$set_inverse(inv)
        # Finally, return the inverse matrix
        inv
}
