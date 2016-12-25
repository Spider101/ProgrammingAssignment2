## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(matr){
        x <<- matr
        inv <<- NULL
    }
    get <- function() return(x)
    setInv <- function(matrInv) inv <- matrInv
    getInv <- function() return(inv)
    return(list(set=set, get=get, setInv=setInv, getInv=getInv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrInv <- x$getInv()
    if(!is.na(matrInv)){
        message("getting cached data")
        return(matrInv)
    }
    data <- x$get()
    matrInv <- solve(data, ...)
    x$setInv(matrInv)
    return(matrInv)
}
