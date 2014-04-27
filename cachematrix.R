## Caching the inverse of a matrix is more efficient than computing the 
##              inverse of matrix repeatedly. Two functions are provided below to 
##              implement a matrix caching object:

## makeCacheMatrix() is a function that creates a matrix object  
##              that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        # inv_mtx is the variable where to store the inverse of the matrix
        # x_local refer to the local copy of the matrix
        
        inv_mtx <- NULL  
        x_local <- x  
        
        # get returns the stored copy of the matrix
        get <- function() x_local
        
        set <- function(y) {
                # set receives a new matrix and stores a copy locally
                x_local <<- y
                # set also clears the stored inverse
                inv_mtx <<- NULL
        }
        
        # getinverse() returns  the stored inverse
        # setinverse() receives the calculated inverse and stores it 
        # These two functions are intended to be used by cacheSolve()
        # to store and retrieve the inverse
        getinverse <- function() inv_mtx
        
        setinverse <- function(calculated_inverse) {
                inv_mtx <<- calculated_inverse
        }
        
        # We return a list of four functions 
        return(list(set = set, 
                    get = get,
                    setinverse = setinverse,
                    getinverse = getinverse))
        
}

## =================================


## cacheSolve() computes the inverse of the matrix 
##              returned by makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## x is a makeCacheMatrix() object defined by the function above
        
        # if x has a stored inverse, we first retrieve it
        stored_inverse <- x$getinverse()
        
        # If we indeed find it has a stored value for the inverse
        # then we simply return it
        if(!is.null(stored_inverse)) {
                message("getting cached data")
                return(stored_inverse)
        }
        
        ## if we have reached this stage, then clearly there was no stored inverse
        
        # We therefore retrieve the stored matrix inside x
        x_local <- x$get()
        
        # calculate the solution (we find the inverse of x_local)
        calculated_inverse <- solve(x_local, ...)
        
        # we store the solution inside the caching object
        x$setinverse(calculated_inverse)
        
        # and we return the solution
        # (since it has been stored, we will not have to calculate it again)
        return(calculated_inverse)
}

