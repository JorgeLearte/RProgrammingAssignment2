## Programming Assigment 2: Jorge Learte
## Create a cache-function with the methods will be used in the function
## which calculates the inverse of a matrix.

## Cache the matrix and assing the methods:
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    if(!require('matlib')){
        install.packages('matlib',method=inv)
        library('matlib')
    }
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if ((nrow(data)==ncol(data))&&(try(det(data)!=0))){
        if (det(data)!=0){
            m <- inv(data) 
        }  
    }
    
    x$setInverse(m)
    m
}
