## makeCacheMatrix creates a 'special matrix', a list of functions that apply to the argument 
## matrix x and allows us to cache the inverse of x. This inverse is computed by cacheSolve, which 
## uses R's solve function to calculate the inverse and then stores it into the 'special matrix'.

## This function takes a (numeric) matrix and creates a list of functions related to that matrix.
## These functions include:
#### set: assigns the argument (y) value to x. This is the method to use when altering 'x'. As we 
####    want to know when x has been modified (in order to re-compute its inverse), inv is reset to 
####    NULL every time this function is called.
#### get: returns the value of x, which might have been modified by set.
#### setinv: assigns the object inverse to inv. This should only be called by cacheSolve, otherwise 
####    any value could be assigned to inv.
#### getinv: returns the value of inv, which is either NULL (before setinv has been called) or the
####    inverse matrix of x.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inverse) {
                inv <<- inverse
        }
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse matrix of x$get(). In order to reduce the computational cost, 
## the function first tests whether the inverse has already been computed for the current x matrix.
## As x$set() resets the variable storing the inverse to NULL every time x changes, we only need to
## test whether x$getinv() exists. If it does, it is simply returned, which saves computational 
## time.
## Otherwise, the inverse is calculated and cached using x$setinv().


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getinv())) {
                message("Getting inverse from cache")
                return(x$getinv())
        }
        inv <- solve(x$get())
        x$setinv(inv)
        inv
}
