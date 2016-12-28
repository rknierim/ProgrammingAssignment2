## To save compute time when performing a common activity of retrieving a matrix's inverse
## the following functions allow one to cache the inverse of a particular "special matrix" object

## The makeCacheMatrix function takes a single matrix argument and creates a "special matrix"
## with internal functions to get/set the matrix values, as well as to get and set the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Set the i object to null as a new matrix is created
        i <- NULL
        
        ## A setter function (if we want to change the x matrix in the future)
        ## This needs to set the newly supplied matrix to the parent x and nullify the parent i
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## A getter function to return the value of x (the current matrix)
        get <- function() x
        
        ## A setter function that places the inverse of x into parent object i
        setinverse <- function(solve) i <<- solve
        
        ## A getter function to retrieve the value of i (the inverse matrix)
        getinverse <- function() i
        
        ## Create/return a list of the functions that may be used in the special matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function takes a "special matrix" and returns the matrix's inverse
## by first checking if the inverse exists in the special matrix, and calculating/saving it if not
cacheSolve <- function(x, ...) {
        ## Attempt to set i from cache
        i <- x$getinverse()
        
        ## If it is cached, let the caller know it was cached and return the inverse matrix
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Otherwise, we need to compute the inverse matrix
        ## First, get the current matrix
        data <- x$get()
        
        ## Next, compute the inverse matrix
        i <- solve(data, ...)
        
        ## Set (cache) the inverse matrix within our special matrix
        x$setinverse(i)
        
        ## Return the inverse matrix
        i
}
