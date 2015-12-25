## The functions take a matrix and return its inverse. If the inverse has
## previously been calculated, they return the cached value. Otherwise, they
## calculate the value.

## makeCacheMatrix is a function containing a list of four other functions.
## It takes as input a matrix and returns functions to:
## get: return the input matrix
## set: change the input matrix (and clear the cached mean)
## getinv: get the inverse (NULL if not cached)
## setinv: set the inverse (called by cacheSolve)

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


## cacheSolve takes as input the list of functions from makeCacheMatrix and
## returns the inverse of the matrix in that list.
## It first tries to load the cached inverse and if that is not available,
## uses the solve() function to calculate the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    return(inv)
}
