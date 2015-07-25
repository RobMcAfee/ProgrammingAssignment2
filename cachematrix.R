## This set of function will allow the creation of an object that will store a matrix
## and cache the inverse matrix.  

## The makeCacheMatrix will create an object that stores a matrix, the inverse
## for that matrix, and functions to set and get the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
        # Initialize inverse matrix
        x_inverse <- NULL
        
        # function to store a new matrix
        set <- function(new_matrix_values) {
                x <<- new_matrix_values
                
                # get rid of any previously stored inverse
                x_inverse <<- NULL
        }

        # function to return the stored matrix
        get <- function() x
        
        # function to store the inverse matrix
        setInverse <- function(inverse_matrix) x_inverse <<- inverse_matrix
        
        # function to return the stored inverse matrix
        getInverse <- function () x_inverse
        
        # return a list that names each function that can be called on the
        # created object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function will find the inverse of a matrix stored in an object created 
## by the makeCacheMatrix function above.  However, it first checks to see if 
## the inverse has already been calculated and cached. If so, it skips the 
## calculation and returns the cached matrix. Otherwise, the function will
## caclulate the inverse and then cache and return the inverse.

cacheSolve <- function(x, ...) {
        # get the cached inverse matrix
        x_inverse <- x$getInverse ()
        
        # if the cached inverse exists, then return it.  No cacluation needed.
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }
        
        # only get to here if the cached inverse is NULL
        # cacluate inverse of x, store the inverse, and return it
        matrix_values <- x$get()
        x_inverse <- solve(matrix_values)
        x$setInverse <- x_inverse
        
        # Return a matrix that is the inverse of 'x'
        x_inverse
}
