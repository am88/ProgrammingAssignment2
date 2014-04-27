## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
    
    inverseMat <- NULL
    setMat <- function(y) {
        mat <<- y
        inverseMat <<- NULL
    }
    getMat <- function() mat
    setInverse <- function(newInverseMat) inverseMat <<- newInverseMat
    getInverse <- function() inverseMat
    list(setMat = setMat, getMat = getMat,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getMat()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
