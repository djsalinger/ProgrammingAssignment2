## makeCacheMatrix and cacheSolve are a pair of functions
## which work together to either cache the inverse of a 
## matrix or retrieve an already cached inverse to avoid
## a redundant calculation 

## makeCacheMatrix is a function which will create
## a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        ## Initializes variable inv
        inv <- NULL
        
        ## Sets the "source" matrix with a supplied argument
        ## and initializes the "target" matrix inverse cache
        ## in the calling function's environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Returns the "source" matrix
        get <- function() x
        
        ## Calculates and sets the matrix inverse cache (inv)
        ## in the calling function's environment
        setinv <- function(solve) inv <<- solve
        
        ## Return matrix inverse cache (inv) if it exists
        getinv <- function() inv
        
        
        ## Return a matrix-type list of functions
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
        
}


## cacheSolve is a function which will check to see if a
## matrix has already had its inverse calculated and cached
## or, if not, will use solve function to calculate and
## cache it for the next call

cacheSolve <- function(x, ...) {
        
        ## Return a cached inverse of the matrix 'x'
        inv <- x$getinv()
        
        ## Check to see if inverse has been calculated
        ## and cached already and will return the
        ## cached value if it has
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Else, get the matrix, calculate its inverse,
        ## set the calculated inverse cache, and
        ## return the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        inv
        
}
