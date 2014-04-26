## The Script is designed to calculate the Inverse of a Matrix. 
## For large matrices, inverse computation is a faily time consuming process. 
## Hence internal memory cache concept is used to obtain the inverse of a matrix
## from cache memory in case it has already been computed.

## The first parent function outputs a list of functions which are called by the second
## parent function.

## Notice the "<<-". This means that when I am assigning a value to object inv,
## this value is assigned to the same 'inv' object defined in the makeCacheMatrix
## and note a seperate 'inv' object inside other sub functions.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## The below fuction is the main function which calls other function.
## It firsts check whether a value of inverse is already present for the input
## The input in this case is a matrix
## IF a value is already present, it just provides the cached value along with the
## comment that cached value has been provided.
## Else it calculates the inverse

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
