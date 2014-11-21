## This functions compute the inverse of the special "matrix"
## and creates a special "matrix" object that can cache its inverse.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix() is passed a matrix which creates an "object" of type 'list'.  
## There are four functions, two to read (or 'get') the value of the two things we are storing, 
## and two functions to change ('set') them.

makeCacheMatrix <- function(x = matrix()) {     # input x will be a matrix
        m <- NULL                               # m will be inverse matrix
        set <- function(y) {                    # set the original matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                     # this function returns the original matrix
        
        setsolve <- function(solve) m <<- solve # this is called during the first cacheSolve()
        
        getsolve <- function() m                # this will return the cached value to cacheSolve()
        
        list(set = set, get = get,... =         # this is accessed each time makeCacheMatrix() is called
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve() accesses the object (not the makeCacheMatrix() function, but the object
## created when makeCacheMatrix() was called) by fetching the value of the matrix used
## to create the object, this matrix being stored when the object was created.

cacheSolve <- function(x, ...) {                # the input x is an object created by makeCacheMatrix
        
        m <- x$getsolve()                       # accesses the object 'x' and gets the value of the solve
        
        if(!is.null(m)) {                       # if mean was already cached (not NULL) ...
                message("Getting cached data...")
                return(m)                       # ... return the inverse matrix ... "return" ends 
        }
        data <- x$get()                         # we reach this code only if x$getsolve() returned NULL
        
        m <- solve(data, ...)                   # if m was NULL then we have to calculate the solve
        
        x$setsolve(m)                           # store the calculated solve value in x
        m                                       # return the solve to the code that called this function
}

