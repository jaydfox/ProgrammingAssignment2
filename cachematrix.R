## The makeCacheMatrix function takes a matrix 'x' and caches the inverse of the matrix
## The cacheSolve function pulles the cached inverse of matrix 'x' from the makeCacheMatrix and gives the output of the inverse of matrix 'x'

makeCacheMatrix <- function(x = matrix()) { ##creates a special matrix object that caches the inverse of the matrix
        m <- NULL ##creates an pairlist to m
        set <- function(y) {
                x <<- y  ##<<- assigns the a value of y to object x
                m <<- NULL ##<<- assigns the value of NULL to object m
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix) ##creates a list containing the values of set, get, setmatix, get matrix
}

cacheSolve <- function(x = matrix(), ...) { ##function that computes the inverse of the special matrix returned by makeCacheMatrix
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
