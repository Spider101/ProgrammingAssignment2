###############################################################################

## Coursera - R Programming Course
## Author: Abhimanyu Banerjee
## Date: 12/25/2016

## File Description:

# This script demonstrates the power of lexical scoping. Lexical Scoping is used
# to create a function constructor to define the special caching behaviour of 
# the matrix whose inverse is to be calculated. The function constructor returns
# a list of four functions:
# 1. set - caches the value of the matrix and initializes its inverse as null
# 2. get - gets the cached value of the matrix
# 3. setInv - caches the inverse (passed to it as an argument) of the matrix
# 4. getInv - gets the cached inverse of the matrix
 
###############################################################################

## This is a contructor function that sets up the caching behaviour of the
## special matrix whose inverse is to be computed (see function after this one)
makeCacheMatrix <- function(x = matrix()) {
    ## Return a list of getter-setter functions for the matrix and its inverse
    inv <- NULL 
    set <- function(matr){
        x <<- matr
        inv <<- NULL
    }
    get <- function() return(x)
    setInv <- function(matrInv) inv <<- matrInv
    getInv <- function() return(inv)
    return(list(set=set, get=get, setInv=setInv, getInv=getInv))
}


## This function computes the inverse of a matrix. The function first checks if 
## the inverse of the matrix was cached previously. If it was, it returns that 
## value, otherwise caches the matrix and its computed inverse for future use.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrInv <- x$getInv()
    if(!is.null(matrInv)){
        message("getting cached data")
        return(matrInv)
    }
    data <- x$get()
    matrInv <- solve(data, ...)
    x$setInv(matrInv)
    return(matrInv)
}
