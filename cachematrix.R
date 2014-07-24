makeCacheMatrix  <- function(x = matrix()) {
    ## creates a special "vector", which is really a list containing a function to
    ## reset the inverse matrix
    ## get the input matrix
    ## set the inverse matrix
    ## get the inverse matrix
    cachedInverse <- NULL
    resetcache <- function() cachedInverse <<- NULL
    getMatrix <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(resetcache = resetcache, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(cm, ...) {
    ## Return a matrix that is the inverse of 'x'
    cInv <- cm$getInverse()
    if(!is.null(cInv)) {
        message("getting cached Inverse Matrix")
        return(cInv)
    }
    matrixdata <- cm$getMatrix()
    inv <- solve(matrixdata)
    cm$setInverse(inv)
    inv
}


