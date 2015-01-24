<!-- -->
        #  The first function, `makeVector` creates a special "vector", which is
        #  really a list containing a function to

        #  1.  set the value of the vector
        #  2.  get the value of the vector
        #  3.  set the value of the mean
        #  4.  get the value of the mean
        
        makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) i <<- solve
                getinverse <- function() i
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("retrieving cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}