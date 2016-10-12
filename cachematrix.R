## The functions below will allow the user to inverse a square matrix and when 
## calling the function again, if the matrix has already been inversed, a 
## cached version of the inverse will be returned.

## Follow the commands below in order to make use of the functions:
## some.matrix <- matrix(c(9,2,3,4,5,6,8,1,7),ncol=3) ## sets up a square matrix
## mat1 <- makeCacheMatrix(some.matrix) ## first call to set up the object
## cacheSolve(mat1) ## call to the cache function

## This function returns a list with set(), get(), setinverse(), and 
## getinverse() functions in it, along with the data from the matrix
makeCacheMatrix <- function(x = matrix(, nrow=2, ncol=2)) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}