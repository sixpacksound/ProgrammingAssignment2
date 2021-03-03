## These functions will store a matrix inversion
## in cached memory to make computations faster.

## First, we create the cached matrix object.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Calculate the inverse and see if it exists in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("Getting cached data...")
                        return(m)
                }
                data <- x$get()
                m <- solve(data,...)
                x$setinverse(m)
                m
}
