## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mat) {
        if(!is.matrix(mat)) stop("Argument must be a matrix")
        invMat <- NULL
        set <- function(y) {
                if(!is.matrix(y)) stop("Argument must be a matrix")
	  mat <<- y
                invMat <<- NULL
        }
        get <- function() mat
        setinverse <- function(inver) invMat <<- inver
        getinverse <- function() invMat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache

cacheSolve <- function(mat, ...) {
        
        invMat <- mat$getinverse()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        data <- mat$get()
        invMat <- solve(data)
        mat$setinverse(invMat)
        invMat
}

