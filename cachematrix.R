## The following functions allow to cache te inverse of the given matrix 
## This operation could be useful for lowering the computational cost of a program 
## in which the inverse of a matrix must be computed repeatedly

## function makeCacheMatrix
##
## description: 
##      This function creates a special object, representing a passed matrix 'x',
##      that can cache its inverse.
##      The returned object is a list of functions.
## arguments:
##      x - a matrix 
##        - it is guaranted to be squared and invertible 
## 


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv_) inv <<- inv_
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}

## function: chacheSolve
## 
## description: 
##              This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##              If the inverse has already been calculated (and the matrix has not changed), 
##              then cacheSolve retrieves the inverse from the cache.
## arguments:
##              x - an object representing the given matrix (which we want to invert) returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    # checking if the inverse of the matrix repesented by the object x hasn't been already computed
    if(is.null(inv)) {
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
    }
    else {
        message("getting cached data")
    }
    inv
}
