## makeCacheMatrix provides functionalities to create and store a matrix and its inverse using 4 functions get, set, setinverse and getinverse
## set will receive a matrix and store it in a variable while keeping its inverse as NULL
## get will return the stored matrix previously saved by the set function 
## setinverse will receive the inverse of the matrix stored by the set function and keep the value in the variable inverse for further consultation
## getinverse will return the value of the inverse variable, either NULL or the inverse of the matrix stored by the set function
## cacheSolve retrieves the value of the inverse variable by calling the getinverse function and verifies if a value is already in the cache or
## it needs to be calculated. If value is already calculated it retrieves it from the stored variable inverse and if the value is missing it calculates
## the inverse with the function solve(), sends the value to be stored and retrieves the inverse of the matrix 

## makeCacheMatrix returns a list with functions to get and set a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(invmatrix) inverse <<- invmatrix
        
        getinverse <- function() inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix either by calculating it or by retrieving it from a stored value

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                return(inverse)
        }
        
        savedmatrix <- x$get()
        
        inverse <- solve(savedmatrix)
        
        x$setinverse(inverse)
        
        inverse
}
