## Functions for caching the inverse of a matrix.
## The makeCacheMatrix function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {     ## define the argument with default mode of "matrix"
        inverse <- NULL                         ## initialize inverse as NULL; will hold value of matrix inverse
        set <- function(y){                     ## define the set function to assign new 
                x <<- y                         ## value of matrix in parent environment
                inverse <<- NULL                ## if there is a new matrix, reset inverse to NULL
        }
        get <- function() x                     ## define the get fucntion - returns value of the matrix argument
        setInverse <- function(solveMatrix){    
                inverse <<- solveMatrix         ## assigns value of inverse in parent environment
        } 
        getInverse <- function() inverse        ## gets the value of inverse where called
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse     
}