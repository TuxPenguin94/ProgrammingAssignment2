## The functions create a matrix and cache the result of the solution
## It helps save time at time-consuming computations

## The function creates a "special" matrix. Actually, it looks like a list with its own environment.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function solves the matrix. If the determinant is equal to 0, it throws an error.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("...Getting cached data...")
                return(s)
        }
        data <- x$get()
	if (det(data) == 0)
		stop("The determinant is equal to 0. Can't solve the Matrix.")
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
