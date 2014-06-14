##################################################
# Solution to programing assignment 2 for the
# R Programming course in the Coursera Data Sciende
# Specialization. This assignment consists of two
# functions 'cacheSolve' which  computes the inverse
# of the matrix returned from a call to 'makeCacheMatrix'
# If the inverse has already been calculated
# (and the matrix has not changed), then
# 'cachesolve' will retrieve the inverse from the cache
# otherwise 'cacheSolve will calculate and cache
# the inverse.
##################################################

##################################################
# makeCacheMatrix
# arguments:
#   a matrix (assumed to be invertible)
# returns:
#   a list of functions -set, get, setinverse, and
#   getinvers - which cache the matrix  and its inverse
#   in the global environment and retrieves the
#   matrix and its inverse from the global environment
##################################################
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##################################################
# cacheSolve
# arguments:
#   a list returned from a previous call to
#   makeCacheMatrix
# returns:
#   the inverse of the matrix cached by
#   makeCacheMatrix
##################################################
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
