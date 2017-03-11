## Put comments here that give an overall description of what your
## functions do

## This function is for programming assignment 2 to 
# cache an inverse of a matrix
#first this function takes and stores the matrix in the corresponding 
#levels of the environment and returns a list
#requires an inversable matrix as argument

makeCacheMatrix <- function(v=rnorm(100), row=10, col=10) {
    x = matrix(v, row, col)
    cache <- NULL

    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setinverse <- function(solveMatx) cache <<- solveMatx
    getinverse <- function() cache
list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## if given result from makeCacheMatrix,
# than this function gets it's inverse or gets a cached inverse 

cacheSolve <- function(x, ...) {
    cache <- x$getinverse()
    if (!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setinverse(cache)
    cache
## Return a matrix that is the inverse of 'x'
}


# To test
mat <- makeCacheMatrix(rnorm(16), 4,4)
mat$get() #retrieve the matrix
mat$getinverse() # should be NULL or location in memory equivalent

cacheSolve(mat) #should return the inverse of mat

mat$getinverse() #should now return cached inverted matrix

