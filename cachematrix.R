## makeCacheMatrix creates a matrix that can be inverted and cached.
## cacheSolve inverts the matrix from makeCacheMatrix and pulls it from the
## cache.


## Creates a matrix in which the inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
		m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
