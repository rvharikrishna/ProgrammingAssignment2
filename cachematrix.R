## Following functions define, retrieve, inverse, and cache the inverse of a matrix
## Hari Rekapalli. 7/24/14.

## "makeCacheMatrix" defines the matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## initialize the inverse
    i <- NULL

    ## set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## get the matrix
    get <- function() {
    	m
    }

    ## set the inverse of the matrix
    setinverse <- function(inverse) {
        i <<- inverse
    }

    ## get the inverse of the matrix
    getinverse <- function() {
        i
    }

    ## list the available methods
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## "cacheSolve" calculates the inverse of the matrix defined in "makeCacheMatrix" above, unless the inverse has been calculated already (if the inverse has been pre-computed, 
## this function will return the inverse from the cache).
cacheSolve <- function(x, ...) {

    ## return inverse of x
    m <- x$getinverse()

    ## return the cached inverse if it has been computed already
    if( !is.null(m) ) {
            message("this is from cache")
            return(m)
    }

    ## get the matrix 
    mat <- x$get()

    ## inverse calculation step
    m <- solve(mat)

    ## set the inverse 
    x$setinverse(m)

    ## return the matrix
    m
}