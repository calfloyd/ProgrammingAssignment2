# Functions provide a method for caching the inverse of a matrix by creating
# a special version of a matrix and the function Solve


# makeCacheMatrix creates a list of functions for
# getting and setting the values of an input matrix (M) 
# and getting and setting the inverse of M

makeCacheMatrix <- function(M = matrix()) {
        I <- NULL
        set <- function(y) {
                M <<- y
                I <<- NULL
        }
        get <- function() M
        setinverse <- function(Inv) I <<- Inv
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# cacheSolve gets the cached inverse of a matrix (M) created
# with makeCacheMatrix or sets the inverse using solve
cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'M'
        Inv <- M$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- M$get()
        Inv <- solve(data, ...)
        M$setinverse(Inv)
        Inv

}
