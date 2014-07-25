## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    get <- function() m
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'm'
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

test <- function(m) {
    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
    amatrix$get()         # Returns original matrix
    cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
    amatrix$getinverse()  # Returns matrix inverse
    cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
    
    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
    cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
    amatrix$get()         # Returns matrix
    amatrix$getinverse()  # Returns matrix inverse    
}
