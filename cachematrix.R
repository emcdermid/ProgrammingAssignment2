## This file provides functions that support caching the expensive
## inverse operation on a matrix (assuming the matrix is invertible).


## Given a matrix x, create a "cache matrix" list that wraps the matrix
## and provide accessor (set/get) methods for the matrix and its inverted
## value. The inverse is calculated externally via cacheSolve() and cached
## here; setting the value of the matrix clears the cache by resetting the
## cached inverse value to null.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Cached value of the inverted matrix
        cached.inv <- NULL
        
        ## Set accessor: set the underlying matrix value, and clear cache
        set <- function(y) {
                ## Set the matrix value
                x <<- y
                
                ## Clear the cached inverse valuez <- makesq <- 
                cached.inv <<- NULL
        }
        
        ## Get accessor: return the underlying matrix
        get <- function() x
        
        ## Set inverse accessor: caches the inverse of the underlying matrix
        set.inverse <- function(inverted) cached.inv <<- inverted
        
        ## Get inverse accessor: gets the cached value of the inverse.
        ## If the function returns NULL, the cache has not been populated.
        get.inverse <- function() cached.inv
        
        ## Create the list that provides access to these functions
        list(
                set = set,
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse
        )
}


## Calculates the inverse of the "cache matrix" x, first checking to see if the
## inverse has already been cached.  If so, the cached value is simply
## returned.  If not, it is calculated, cached, and then returned.
##
## Note: the matrix is assumed be invertible; no validation is performed. 

cacheSolve <- function(x, ...) {
        ## Check whether the inverse has already been calculated
        inverted <- x$get.inverse()
        
        ## If the returned value isn't null, then it has been cached.  Return it.
        if(!is.null(inverted)) {
                return(inverted)
        } 
        
        ## If we've reached this point, there was no cached value, so calculate it.
        original <- x$get()
        inverted <- solve(original)
        
        ## Cache the calculated value
        x$set.inverse(inverted)
        
        ## Return the calculated value
        inverted
}
