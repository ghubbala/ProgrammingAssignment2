## This function creates a list of functions to do the following 
## set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse of the matrix
## Get the value of the inverse of the matrix
## And stores the inverse of the matrix in matrix variable called invMat
makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invMat <<- inverse
        getinverse <- function() invMat
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of the matrix
## If the inverse is cached then cached value is returned
## Else inverse is computed using 'solve' and returns
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getinverse()
        if(!is.null(invMat)) {
                message("Get the inverse matrix from cache")
                return(invMat)
        }
        mtrx <- x$get()
        invMat <- solve(mtrx)
        x$setinverse(invMat)
        invMat        
}
