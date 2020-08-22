
## makeCacheMatrix
## initial line establishes input argument x (matrix class)
##  m is initiated for use later as object for calculated inverse matrix; it is made NULL to erase any prior saved value(s) within the makeCacheMatrix environment
## the set function sets new input as x, and also erases any prior value of “m” in the parent environment by use of the <<- operator
## the “get” function calls the input argument 
## the list generated at the end makes the functions set, get, setInvMtrx, getInvMtrx accessible from global environment (and can be accessed from the parent environment via the $ extractor, as done by cacheSolve


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvMtrx <- function(InvMtrx) m <<- InvMtrx
        getInvMtrx <- function() m
        list(set = set, get = get, setInvMtrx = setInvMtrx, getInvMtrx = getInvMtrx)
}


## Summary- cacheSolve called with a single argument; in this case, the input must be an output from makeCacheMatrix (i.e. so that it includes “set,” “get,” “setmean” and “getmean” functions
## if there is a cached value of m is available via the x$getmean line (
## (i.e. is NOT null, it will activate the "if" statement and retrieve m, and use return to make it available in the parent environment
## if no cached value present, the next 3 lines will calculate a new inverse matrix: 
##  - the x$get (accessed from "makeCacheMatrix" function's environment)
##  - the "solve" function will calculate a new inverse
##  - x$setInvMtrx function will use the accessed “setInvMtrx” function to cache this newly calculated value “m”, to be saved to parent environment via <<- operator
##      - this is represented by “m” within the cacheSolve function, and serves as the input for argument “InvMtrx” input for setInvMtrx within the makeCacheMatrix environment
## the program will commence with auto-printing output value "m"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvMtrx()
        if(!is.null(m)) {
                message("accessing cached value”)
                return(m)
        } else {
	message("no cached value- generating new value")
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMtrx(m)
        m
        }
}
