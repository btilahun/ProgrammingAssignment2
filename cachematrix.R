## The below two functions will cash and compute the inverse of a matrix 
## The "makeCacheMatrix" function will cash the matrix and "cacheSolve  "
## function will compute the inverse of the matrix 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y;
        m <<- NULL;
    }
    get <- function() x;
    setinv <- function(v) m <<- v;
    getinv <- function() m;
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special 
## "matrix" returned by "makeCacheMatrix" function. 
## If the inverse has already been calculated and no 
## change in the matrix the function will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
   
   ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
    if(!is.null(m)) {
        message("Getting data from cache...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
