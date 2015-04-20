## The functions defined here build an extended matrix datastructure
## so that repeatedly calculations of the inverse can immedieatly 
## be taken from that structure. 

## This function creates a special "matrix" object that can cache its inverse.
## It's a kind of a constructor for an object of a class (in the sense of an 
## object oriented programming language). 
## Data:
## x - invertible square matrix
## xinv - inverse of x

makeCacheMatrix <- function(x = matrix()) {
    ## x and xinv are defined in the scope of funtion makeCacheMatrix.
    ## Using <<- in the functions will change these variables.
    xinv <- NULL
    
    ## set sets the matirx x cached here and deletes the value of xinv. 
    ## Parameter mat must be an invertable square matrix.
    set <- function(mat) {
        xinv <<- NULL
        x <<- mat
    }
    
    ## get just returns the currentvalue of x
    get <- function() x
    
    ## You can calulate the inverse on your own and set it with setinv, but 
    ## nothing prevents you from setting anny other value.
    setinv <- function(inv) xinv <<- inv
    
    ## get inv just returns the current value of xinv.
    getinv <- function() xinv
    
    return(list(set=set, 
                get=get, 
                setinv=setinv, 
                getinv=getinv))
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(is.null(inv)) {
        message("computing inverse ...")
        inv <- solve(x$get(), ...)
        x$setinv(inv)
    } 
    return(inv)
}
