## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
## We assume that the matrix supplied is always invertible. Solve(X)
makeCacheMatrix <- function(x = matrix()) {
        # 1. Set the value of the matrix
        inv <- NULL
        set <- function(A) {
                x <<- A
                inv <<- NULL
        }
        
        # 2. Get the value of the matrix
        get <- function() x
        
        # 3. Set the value of the inverse
        set_inverse <- function(inverse) inv <<- inverse
        
        # 4. Get the value of the inverse
        get_inverse <- function() inv
        
        # The special vector created is a list containing a function for the 4 above steps
        list(set=set, 
             get=get,
             set_inverse=set_inverse,
             get_inverse=get_inverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # Check if the inverse has already been calculated. If so, get the inverse from the cache.
        inv <- x$get_inverse()
        if(!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        # If the inverse has not been calculated, it calculates the inverse of the data
        data <- x$get()
        inv <- solve(data, ...)
        # the value of the inverse is set in the cache
        x$set_inverse(inv)
        inv
}
