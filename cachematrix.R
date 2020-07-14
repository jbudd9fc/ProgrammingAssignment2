## Given the speed implications of inverting a matrix on demand, it can be 
## beneficial, if it remains constant, to store the output of this calculation
## to be pulled from the environment rather than calculated each time the 
## the function is run. These functions first, make a matrix and cache its 
## inverse and secondly, compute the inverse if it is not already cached

## This first function creates a matrix. This matrix is a list with the set and 
## get the value of matrix and the set and get value of the inversion.

makeCacheMatrix <- function(x = matrix()) {
        y <- NULL
        set <- function(z) {
                x<<-z
                y<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) y <<- inverse
        getinverse <- function() y
        list(set=set,get=get,
             setinverse = setinverse, 
             getinverse = getinverse)
}

## This function inverts the matrix created in the first function. However, 
## it first checks to see whether the matrix has already been inverted. If 
## so it retrieves the inversion from the cache

cacheSolve <- function(x, ...) {
        y <- x$getinverse()
        if(!is.null(y)) {
                message ("getting cached data")
                return(y)
        }
        data <- x$get()
        y <- solve(data,...)
        x$setinverse(y)
        y
        ## Return a matrix that is the inverse of 'x'
}
