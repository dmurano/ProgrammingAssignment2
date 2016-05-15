## Matrix inversion is usually a costly computation and here are a pair of 
## functions that cache the inverse of a matrix.
## Thanks for grading my assignment!

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Set the matrix inverse to NULL
        minv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                minv <<- NULL
        }
        ## Gets the value of the matrix
        get <- function() x
        ## Calculates the inverse
        setinv <- function(solve) minv <<- solve
        ## Gets the inverse
        getinv <- function() minv
        ## Passes value of function
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## In order to avoid the message #Error in x$getinv : $ operator is invalid for atomic vectors
        ## you need to correct the input parameter of cacheSolve as follows:
        ## First: m<-makeCacheMatrix(thematrixofyourchoice)
        ## Second: cacheSolve(m)
        minv <- x$getinv() 
        ## Obtains the inverse if already there
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        ## Otherwise it calculates the inverse
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}
