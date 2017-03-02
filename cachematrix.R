## Cache matrix inversion

## Returns a list of functions to set and get input matrix and its inversion

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) I <<- Inv
    getInv <- function() I
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Ruturns inverse matrix -- checks if inverted matrix already available or
## not -- either cache or calculate the inversion matrix

cacheSolve <- function(x, ...) {
    I <- x$getInv()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInv(I)
    I
}
