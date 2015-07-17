## The makeCacheMatrix function create an object that can contain a matrix and its inverse.
## The cacheSolve function can  calculate its inverse, drawing on
## a cached value of the inverse to save computation time
## Andrew Laing - Programming Assignment 2 - July 2015

## makeCacheMatrix creates an object with a list of functions that can contain values
## such as the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL #ensure this value is NULL to begin with
  #define the matrix using a call to SetMatrix
  SetMatrix <- function(y){
    x <<- y
    mInverse <<-NULL
  }
  GetMatrix <- function() x # return the matrix object
  
  SetInverse <- function(i){
    mInverse <<- i # sets the inverse matrix variable
  }
  GetInverse <- function() mInverse # gets the inverse matrix variable
  list(set=SetMatrix,
       get=GetMatrix,
       setInverse=SetInverse,
       getInverse=GetInverse)

}


## This function returns a matrix that is the inverse of x (a matrix), but first checks
#' to see if the inverse was previously calculated. If it is, it returns it from memory.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # get the Inverse matrix(already stored)
  if(!is.null(inv)) {
    message("getting cached data") # as the value is stored
    return(inv)
  }
  #if the Inverse is not already stored, then calculate it
  matrix <- x$get() # get the matrix data
  inv <- solve(matrix) # calculate the inverse
  x$setInverse(inv) # Set the inverse in the matrix object
  inv # return the inverse
}