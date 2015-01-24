## A pair of 'special' functions, makeCacheMatrix and cacheSolve have been created.  
## They allow us to cache the inverse of a matrix.

## The makeCacheMatrix function creates a matrix object that can cache its inverse.
## It allows us to :-
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## When we run this function we can choose to store it afterward in a variable, say 'm'.
## We may then set any SQUARE, INVERTABLE MATRIX in m such as an example command ...
## ... m$set(matrix(1:4,2,2))

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


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
## It will first determine if the inverse has already been computed.
## If the inverse already exists, it will forgo the computation and retrieve the inverse from the cache.
## If the inverse does not exist, it will calculate the inverse of the matrix and set the value of the inverse
## in the cache via the setinverse function.
## By running the cacheSolve function as cacheSolve(m) we the inverse is either retrieved or computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("retrieving cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
