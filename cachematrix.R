## Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly


## This function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Create a special "matrix" object
        ## Used as input argument for cacheSolve function
        
        ## Set the value of the matrix
        ## Get the value of the matrix
        ## Set the value of the inverse
        ## Get the value of the inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ## List to be returned containing the function to "set and get"
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}          




## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Uses special 'matrix' object which is returned 
        ## by makeCacheMatrix function
        
        ## Check to see if matrix has alredy been calculated
        ## via 'getinverse', if so it retrieves the mean from 
        ## the cache
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Compute the inverse of the matrix and
        ## sets the inverse of the matrix via 'setinverse'
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
