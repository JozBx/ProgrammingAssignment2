## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function to create a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL                             ## inv is the var containing the inverse of the matrix         
                                                ## Set the inverse of the matrix to be NULL
        
        set <- function(y) {                    ## this function update the old matrix to be the new matrix
                x <<- y                         ## Set the inverse value (inv) of the new matrix to be NULL
                inv <<- NULL
        }
        
        get <- function() x                     ## Get the actual Matrix
        setinv <- function(solve) inv <<- solve ## Set the value of the inverse of the matrix
        getinv <- function() inv                ## Get the inverse of the matrix
        list(set = set, get = get,              ## Set names in a list
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## function to create the inverse of the cacheable mtrix created thanks to the makeCacheMatrix above

cacheSolve <- function(x, ...) {                ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()                       ## Hold the inverse of the matrix
        if(!is.null(inv)) {                     ## First step: Check if the inverse has already been calculated
                message("getting cached data")  ## If yes, print the message in parentheses...
                return(inv)                     ## ... and return the value of the calculated inverse of the matrix
        }
        
        data <- x$get()                         ## If the inverse hasn't been calculated, get the actual matrix
        inv <- solve(data, ...)                 ## Calculate the inverse using the function solve   
        x$setinv(inv)                           ## Update the variable containing the inverse of the matrix...
        inv                                     ## ...and print that variable
        
}
