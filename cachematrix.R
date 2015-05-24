## This function does the following:
## Set the inverse of a matrix and cache it
## Get the inverse of a matrix, if the inverse in cache then it gets it from cache, else computes and 
## then set it in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of the matrix
## If the inverse is cached then cached value is returned
## Else inverse is computed using 'solve' and returns
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Get the inverse matrix from cache")
                return(m)
        }
        mtrx <- x$get()
        m <- solve(mtrx)
        x$setinverse(m)
        m        
}
cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    ##m <- x[2,2] ##$getinverse()
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Get the data from cache")
        return(m)
    }
    ##mtrx <- x[1,2] ##$get()
    mtrx <- x$get()
    m <- solve(mtrx)
    ##x[2,1](m) ##$setinverse(m)
    x$setinverse(m)
    m        
}
