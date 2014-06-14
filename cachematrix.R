## makeCacheMatrix: This function creates a special "matrix" object
##                  that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix"
##             returned by makeCacheMatrix above.
##             If the inverse has already been calculated (and the matrix
##             has not changed),
##             then the cachesolve should retrieve the inverse from the cache.




## creat a special matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve
        #get the value of the inverse
        getmean <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## calculate the inverse of the special matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        #check to see if the inverse has already been calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
