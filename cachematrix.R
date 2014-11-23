source("cachematrix.R")

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {    ##x is a matrix
    m <- NULL    ##initializes m, which will store the inverse of matrix x
    set <- function(y) {    ##takes input matrix 
        x <<- y    ##saves the input in the global environment
        m <<- NULL    ##resets the inverse to NULL
    }
    get <- function() x     ##returns the value of the original matrix x
    setinvert <- function(invert) m <<- invert    ##called in cacheSolve() on first access and stores the value using superassignment
    getinvert <- function() m    ##returns the cached value to cacheSolve() on subsequent access
    list (set = set, get = get, setinvert = setinvert, getinvert = getinvert)    ##accessed each time makeCacheMatrix() is called when making a new object. It lists the internal functions so that a calling function knows how to access the methods.
}


## cacheSolve returns a matrix that is the inverse of matrix x

cacheSolve <- function(x, ...) {    #the input is the matrix created by makeCacheMatrix()
    m <- x$getinvert()    ##accesses object 'x' and gets the invert of the matrix x
    if(!is.null(m)) {     ##if the invert was already cached, then...
        message ("getting cached data")    
        return(m)         ##it returns the inverted matrix and ends cacheSolve()
    }
    data <- x$get()    ##if m is NULL, then...
    m <- solve(data, ...)    ##we calculate the inverse
    x$setinvert(m)    ##store the calculated inverse
    m    ##return the inverse 
}

