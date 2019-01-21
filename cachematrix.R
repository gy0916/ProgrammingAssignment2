## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Use matrix as an input, set the matrix, get the matrix, and
## calculate the inverse matrix, get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}

##Check if there is any inverse matrix or not.  If there is an inverse matrix
##cache it, if not, generate a new inverse matrix.

cachesolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
