## cachematrix.R
## David Williams: sir.lancelot@kiesbar.com
##
## Two functions below provide a wrapper for matrices such that a matrix inverse
## can be computed and cached for future use rather than repeatedly calculated.
## To create a new matrix with wrapper, call "mat <- makeCacheMatrix(m)" passing 
## the matrix to be wrapped as 'm' to the function.  To set the wrapper to 
## another matrix, use "mat$makeCacheMatrix(new_m)" where 'new_m' is the new 
## matrix to be wrapped.
##
## To compute the inverse of the matrix, call "cacheSolve(mat)" on the wrapped
## mnatrix.  If the inverse has already been computed, the function will return
## the cached inverse.  Otherwise it will compute and cache the result.
## 
## "mat$setInverse(m)" is not intended to be called from any other function than
## "cacheSolve(m)" and has been disabled to return an error message since
## calling it directly could break the wrapping system.  "mat$getInverse()" is
## also not intended for calling outside of "cacheSolve(m)" but is not disabled
## since calling it will not break the wrapping syste.

## makeCacheMatrix( matrix() )
## Wrapper function for matrices and their inverses to allow setting and getting
## wrapped matrices and their inverses.
makeCacheMatrix <- function(which_matrix = matrix()) {
        inverse <- NULL
        # $set( matrix() ) sets the wrapper to point at a new matrix.
        set <- function (new_matrix) {
                which_matrix <<- new_matrix
                inverse <<- NULL
        }
        # $get() returns the wrapped matrix.
        get <- function() which_matrix
        # $setInverse( matrix() ) is called by cacheSolve( c-matrix ) to set the 
        # computed inverse of the wrapped matrix.  If $setInverse( matrix() )
        # is called directly, an error is returned.
        setInverse <- function(inverse) {
                if (sys.nframe() > 1) {
                        inverse <<- inverse
                } else {
                        message("Can not set inverse directly.  Use cacheSolve()")                
                }
        }
        # $getInverse() returns the wrapped inverse.  Can return NULL if no 
        # inverse has been yet computed!
        getInverse <- function() inverse
        # References to wrapper functions.
        list(set = set, get = get,
             setInverse = setInverse, 
             getInverse = getInverse)
}

## cacheSolve( c-matrix )
## Utility function for wrapped matrices and their inverses which computes or
## retrieves the inverse of a a wrapped matrix.
cacheSolve <- function(which_matrix) {
        # Retrieve the inverse.
        inverse <- which_matrix$getInverse()
        # If the inverse has been previously computed, return that result.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # If the inverse has not been previously computed, compute it.
        data <- which_matrix$get()
        m <- solve(data)
        # Cache and return the computation result.        
        which_matrix$setInverse(m)
        m
}
