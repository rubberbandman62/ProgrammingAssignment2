## The functions defined here build an extended matrix datastructure
## so that repeatedly calculations of the inverse can immedieatly 
## be taken from that structure. 

## This function creates a special "matrix" object that can cache its inverse.
## It's a kind of a constructor for an object of a class (in the sense of an 
## object oriented programming language). The object defines the data x and xinv
## and the corresponding getter and setter methods. The returned list encapsules
## the data and the methods using that data.

makeCacheMatrix <- function(x = matrix()) {
    ## x and xinv are defined in the scope of funtion makeCacheMatrix
    ## Using <<- in the getter and setter functions will change these variables
    xinv <- NULL
    
    ## set sets the matirx x cached here. 
    ## Parameter mat must be an invertable square matrix.
    set <- function(mat) {
        xinv <<- NULL
        x <<- mat
    }
    
    ## get just returns the cached matrix x
    get <- function() x
    
    ## You can set the inverse your own but no one prevents you
    ## from setting any other value. Your value is stored in xinv.
    ## To avoid trouble I would ommit this function. But in case you have a
    ## more efficient methode to calculate the invese of a matrix you ca use
    ## setinv.
    setinv <- function(inv) xinv <<- inv
    
    ## If getinv is called the first time and you haven't set xinv to your own
    ## value via setinv, getinv will calculate the inverse of x using solve and  
    ## store the result in xinv, otherwise (getinv was allready called before or 
    ## you have set a value via setinv) getinv will just return xinv.
    getinv <- function() {
        if(is.null(xinv)) {
            message("computing inverse in getinv ...")
            xinv <<- solve(x)
        }
        return(xinv)
    }
    
    return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## If implemented follwing the given pattern, this function should look
    ## like:
    # inv <- x$getinv()
    # if(is.null(inv)) {
    #     message("computing inverse my own ...")
    #     inv <- solve(x$get(), ...)
    #     x$setinv(inv)
    # } 
    # return(inv)
    ## This is all unneccessary because I implemented the logic in getinv
    ## function of makeCachMatrix.
}
