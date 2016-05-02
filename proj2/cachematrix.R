## This file uses memoization (http://en.wikipedia.org/wiki/Memoization)
## to implement a cached version of a funciton to compute matrix inversion.
## If the input matrix has not changed since the last time the inverse was 
## calculated, we do not recompute the inverse and instead return the cached
## version.  If the matrix does change, we will recompute the inverse.

## makeCacheMatrix(matrix)
## wrapper around a standard R matrix, proving getter and setter methods,
## and getinverse and setinverse methods to store a cached version of the 
## inverse.  If the underlying matrix is changed via its setter, the cached
## inverse is invalidated

makeCacheMatrix <- function(m = matrix()) {
        inverse <- NULL
        set <- function(y) {
                m <<- y
                inverse <<- NULL
        }
        get <- function() m
        setinverse <- function(mtx) inverse <<- mtx
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(m, ...) {
        inv <- m$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv
}

## Test function to exercise above code.  Would be better written as a unit test
## using RUnit.
## A matrix multiplied by it's inverse will equal the identity matrix.
## 
## for a 2x2 matrix this will look like
## getting cached data
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1

test <- function() 
{
    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
    amatrix$get()         # Returns original matrix
    cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
    amatrix$getinverse()  # Returns matrix inverse
    cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
    
    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
    cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
    amatrix$get()         # Returns matrix
    amatrix$getinverse()  # Returns matrix inverse    
    
    amatrix$get() %*% amatrix$getinverse() # returns the identity matrix
}
