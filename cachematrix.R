##These functions are calculate an matrix inverson and put it on cache. Or get it from cahe when is needed


## This function makes special matrix object, that may cache an matrix inversion
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}



##This function calculates an inversion of matrix
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
