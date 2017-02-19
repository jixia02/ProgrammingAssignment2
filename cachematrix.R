## Put comments here that give an overall description of what your
## functions do

### Write a short comment describing this function
### a pair of functions ( makeCacheMatrix and cacheSolve) cache the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## return a list containing functions to
##      set the matrix
##      get the matrix
##      set the inverse
##      get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL   
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinv <- function(solve) m <<- inverse
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
        # Return a matrix that is the inverse of 'x'
        m <- x$getinv()   
        
        # if the inv is available and has been calculated, get it from cached data
        if(!is.null(m)){      
                message("getting cached data")
                return(m)
        }
        
        # get the inverse
        data <- x$get()
        m <- solve(data,...)
        
        # set the value of the inverse in the cache via the setin function
        x$setinv(m)
        m
}

