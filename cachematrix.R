## The purpose of this function is to cache the inverse
## of a matrix to make duplicate computation unnecessary

## This function creates a list of pointers to functions 
## to be used in the cache function. These are a group 
## of setters and getters for the input matrix and its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        minv = NULL
        set <- function(y){
                x <<- y
                minv <<- NULL
        }
        get <- function()x
        setinv <- function(inverse)minv <<- inverse
        getinv <- function()minv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function first determines whether the result 
## has been cached. If so it returns the cached result.
## If not, it calculates, returns and caches the result from 
## the input matrix.

cacheSolve <- function(x, ...) {
        minv <- x$getinv()
        if(!is.null(minv)){
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data,...)
        x$setinv(minv)
        minv
        ## Return a matrix that is the inverse of 'x'
}
