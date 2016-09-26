## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Sets the value of the matrix to 'x', gets the value 
## of the matrix, sets the value of the inverse, gets the value of
## the inverse. 


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)

}


## cacheSolve: First, returns the inverse if it has already been calculated.
## If not, takes the output of makeCacheMatrix and calculates the 
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
