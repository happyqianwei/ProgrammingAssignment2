## In the following are two functions that are used to create a special object that stores a matrix and 
## cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) inv_m <<- solve
        getInverseMatrix <- function() inv_m
        list(set=set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## It first checks to see if the inverse matrix has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the inverse of the matrix in the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {
        inv_m <- x$getInverseMatrix()
        if(!is.null(inv_m)) {
                message("getting cached inversed matrix")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data)
        x$setInverseMatrix(inv_m)
        inv_m
}
