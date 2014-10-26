## Programming Assignment 2
## First function named makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Second function named cacheSolve returns inverse of metrix returned by makeCacheMatrix.

## This function creates a special "matrix" object that can cache its inverse.
## In this function:
## 1. Set the values for matrix (if it wasn't used previously - set NULLs)
## 2. Get matrix
## 3. Set inverse for this matrix
## 4. Get inverse fot matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(inverse) m <<- inverse
        getinvmatrix <- function() m
        list(
                set = set,
                get = get,
                setinvmatrix = setinvmatrix,
                getinvmatrix = getinvmatrix
        )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## In this function:
## 1. Return matrix which is inverse of x (function getinvmatrix)
## 2. Check if this function was already runned
## 3. If yes, return the matrix : return(m)
## 4. If not, get the value of input function, calculate invers and return matrix

cacheSolve <- function(x, ...)
        m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmatrix(m)
        m
}
