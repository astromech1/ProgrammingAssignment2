# Assignment: Caching the Inverse of a Matrix
#-------------------------------------------------------------------------------------
# Matrix inversion is usually a costly computation and there may be some benefit 
#   to caching the inverse of a matrix rather than computing it repeatedly 
#   (there are also alternatives to matrix inversion that we will not discuss here).
#   Your assignment is to write a pair of functions that cache the inverse of a 
#   matrix.
#-------------------------------------------------------------------------------------
# makeCacheMatrix 
# This function creates a matrix object that can "cache" its inverse.
#
#       Input: Square Matrix        
#	
#	Functions: 
#       -setMatrix - Sets and stores a matrix
#       -getMatrix -  Gets and returns the stored value of the matrix
#       -setInverse - Sets the inverse of the matrix in the "cache" 
#               of the given matrix
#       -getInverse - Geta the cached inverse value of the supplied matrix
# 
#       Returns: list of functions
#
#-------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        # holds the cached value or NULL if nothing is cached
        # initially there is nothing cached so set cache to NULL
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # no longer needed, flush the cache variable
                cache <<- NULL
        }
        
        # returns the stored matrix
        getMatrix <- function() {
                x
        }
        
        # cache the given argument 
        setInverse <- function(solve) {
                cache <<- solve
        }
        
        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, getInverse = getInverse)
        
}
#-------------------------------------------------------------------------------------
# cacheSolve
# This function returns the inverse of the matrix if available.  The function works
#        through the following steps: 
#	1. It checks if the inverse has already been computed. If available 
#		it gets the result and skips the computation. 
#	2. If not, it computes the inverse, sets the value in the cache via
# 		setinverse function in makeCacheMatrix.
#       3. Return the inverse matirx
# 
# 	Returns: a matrix that is the inverse of 'x'
#
# --This function assumes that the matrix is always invertible--
# 
#------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        # get the cached value of the matrix if it is available
        inverse <- x$getInverse()
        
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("Accessing cached data")
                return(inverse)
        }
        # otherwise get the provided matrix 
        data <- x$getMatrix()
        
        # caclulate the inverse
        inverse <- solve(data)
        
        # sets the value in the cache via setInverse
        x$setInverse(inverse)
        
        # return the inverse
        inverse
}

#-------------------------------------------------------------------------------------
# Test & verification
#-------------------------------------------------------------------------------------
#
# create a square matrix
#> sqrMtrx <- c(0, 0, 0, 0, 1, 
#+              1, 0, 0, 0, 0, 
#+              0, 1, 0, 0, 0, 
#+              0, 0, 1, 0, 0, 
#+              0, 0, 0, 1, 0) 
#> sqrMtrx <- matrix(sqrMtrx, ncol=5, byrow=TRUE) 
#> x<-makeCacheMatrix(sqrMtrx)
#> x$getMatrix()
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    0    0    0    0    1
#[2,]    1    0    0    0    0
#[3,]    0    1    0    0    0
#[4,]    0    0    1    0    0
#[5,]    0    0    0    1    0
# 
# Use solve() to verify our results
#
#> solve(sqrMtrx)
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    0    0    0
#[2,]    0    0    1    0    0
#[3,]    0    0    0    1    0
#[4,]    0    0    0    0    1
#[5,]    1    0    0    0    0
#
# Not in the cache at this point
#> cacheSolve(x)
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    0    0    0
#[2,]    0    0    1    0    0
#[3,]    0    0    0    1    0
#[4,]    0    0    0    0    1
#[5,]    1    0    0    0    0
#
# The previous run loaded the matrix in the cache and should
#       be accesible now
#> cacheSolve(x)
#Accessing cached data
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    0    0    0
#[2,]    0    0    1    0    0
#[3,]    0    0    0    1    0
#[4,]    0    0    0    0    1
#[5,]    1    0    0    0    0
