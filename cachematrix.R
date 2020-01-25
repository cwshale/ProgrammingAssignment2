## 'makeCacheMatrix' and 'cacheSolve' create and interact with two cached variables that are intended to hold an
## input matrix and its inverse.

## 'makeCacheMatrix' creates a list of functions to interact with a matrix input and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(z){        # Function to change value of matrix after initially stored into a variable
                x <<- z            # Change matrix value to argument of 'set' and cache it 
                inv <<- NULL       # Reset value of inverse
        }
        
        get <- function() x             # Function to retrieve value of matrix from cached environment
        set_inv <- function(newval) inv <<- newval        # Function to manually set value of 'inv' 
        get_inv <- function() inv       # Function to retrieve value of matrix from cached environment
        
        cache_inv <- function(){        #Calculates the inverse of matrix and stores in cached environment
                inv <<- solve(x)
        }
        
        list(set = set, get = get,      # Creates tags for above expressions to be called using 'x$'
             setinv = set_inv, 
             getinv = get_inv,
             cacheinv = cache_inv)
        }


## 'cacheSolve' utilizes the functions defined in 'makeCacheMatrix' to check if a cached inverse for the input matrix 
## exists. If so, it will return the cached value. If not, it will calculate the inverse and cache the result. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()       #Retrieves cached value of 'inv' for evaluation
        if(!is.null(inv)) {     # If 'inv' has a value...
                message("getting cached data")  # Message to user 
                return(inv)     # Return current cached value and end execution
        }
        y <- x$get()    # Temporary variable equal to cached matrix 
        inv <- solve(y, ...)    # Calculate inverse of cached matrix
        x$setinv(inv)   # Cache the inverse using relevant command from 'makeCacheMatrix'
        inv     # Display value of inverse in console
}
