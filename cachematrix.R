## This is an R file that contains two functions
## needed to complete the programming assignment
## for the Coursera course "R Programming"
## For this assignment we need to cache the inverse
## of a matrix

## "Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly 
## The assignment is to write a pair of functions that 
## cache the inverse of a matrix"


## First function 
##
## the function makeCacheMatrix() creates a special 
## "matrix" object that can cache its inverse.
## for this assignment we will assume that all matrices
## are inversible

makeCacheMatrix <- function(x = matrix()) {
        ## the variable m will correspond to the inverse
        ## value of the input matrix
		m <- NULL
		set <- function(y) {
			    x <<- y
				m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set = set, get = get,
		        setinverse = setinverse,
				getinverse = getinverse)
}

## Second function
##
## the function cacheSolve computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
		
		## if the inverse of the matrix exists
        ## return the cached data
		if(!is.null(m)) {
			    message("getting cached data")
				return(m)
		}
		else {
		        data <- x$get()
		        m <- solve(data, ...)
		        x$setinverse(m)
		}
		m
}

