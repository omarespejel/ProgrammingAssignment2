## As we know taking the inverse of a matrix can be a costly computation if you
## intend to do it repeatedly. So the next pair of fucntions will help us cache 
## the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        i <- NULL ## "i" represents the inverse.
        
        setMatrix <- function(y = matrix()) ##sets a matrix
        {
                x <<- y        #updates the value of x with the given new "y"
                i <- NULL
        }
        
        getMatrix <- function() {x}  #gives the actual value of x
        setInverse <- function(inverse) {i <<- inverse}  #will be use in the function cacheSolve to set the value of the inverse
        getInverse <- function() {i} #retrieves the actual value of the inverse
        
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,  
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...)
{
        i <- x$getInverse()
        
        if (!is.null(i)) #If this value is true then it will proceed to print the already stablished inverse.
        {
                message("getting cache data")
                return(i)
        }
        data <- x$getMatrix() #get the value of the matrix x
        i <- solve(data, ...) #get tjhe inverse of the matrix 
        x$setInverse(i)       #updates the value of the inverse with the new one 
        i
}