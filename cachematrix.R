## Code to create a "special" matrix whose inverse can be cached and used without recomputation
## as long as the matrix remains the same
## Assumptions:
## 1. The matrix supplied is always invertible.

## Definition of a function that provides a list of 4 functions
## 1. Function to get the actual matrix data
## 2. Function to set the matrix data
## 3. Function to get the cached inverse of the matrix
## 4. Function to set(cache) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Given a special matrix created using the makeCacheMatrix function above, 
## this function gets the inverse of the matrix
## 1. either by returning the cached value if there is one,
## 2. or by actually calculating and caching it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
