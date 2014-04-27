## makeCacheMatrix creates an special matrix (from standard R matrix) and returns a list of useful 
## functions, and holds a cache memory for the inverse matrix. 
## Functions: 
## setMat: set a new matrix, reseting the previous inverse matrix.
## getMat: get the current matrix.
## setInverse: set a new value for inverse matrix, modifying the previous cache.
## getInverse: get the inverse matrix value.

## Example:
## > matrixCache <- matrix(c(1,0,1,2,4,0,3,5,6), 3, 3)
## > m <- makeCacheMatrix(matrixCache)
## > cacheSolve(m)
## [,1]        [,2]        [,3]
## [1,]  1.0909091 -0.54545455 -0.09090909
## [2,]  0.2272727  0.13636364 -0.22727273
## [3,] -0.1818182  0.09090909  0.18181818

makeCacheMatrix <- function(mat = matrix()) {    
    inverseMat <- NULL
    setMat <- function(y) {
        mat <<- y  # mat and inverseMat are modified from another enviorment.
        inverseMat <<- NULL
    }
    getMat <- function() mat
    setInverse <- function(newInverseMat) inverseMat <<- newInverseMat
    getInverse <- function() inverseMat
    list(setMat = setMat, getMat = getMat,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve calculates the inverse matrix of a special cacheMatrix matrix, checking before 
## whether the value of inverse matrix has been previously calculated. If so, skip computation and
## returns previous value. Otherwise, call R solve() function.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getMat()
    m <- solve(data, ...)
    x$setInverse(m)  # at this point the cache is updated with the new value.
    m
}
