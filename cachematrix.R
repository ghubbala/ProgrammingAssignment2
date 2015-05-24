## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- cacheSolve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        rbind(c(set = set, get = get), c(setinverse = setinverse, getinverse = getinverse)) 
}


## This function returns the inverse of the matrix
## If the inverse is cahed then chached value is returned
## Else inverse is computed and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x[2,2]() ##$getinverse()
        if(!is.null(m)) {
                message("Get the data from cache")
                return(m)
        }
        mtrx <- x[1,2] ##$get()
        m <- solve(mtrx)
        x[2,1](m) ##$setinverse(m)
        m        
}
