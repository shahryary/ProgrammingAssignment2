# Peer Graded Assignment: Programming Assignment 2: Lexical Scoping

## This code is my solution to second assignments from the R programming on Coursera.

## It's better to cache the results when we running time consuming computations 
## instead of calculate it agian.

## According to the course in Coursera and content in week 3 we see the computing matrix is costly, 
## so we can compute it and then cache the inverse of matrix. 

## So, our first function as in templete is called "makeCacheMatrix" that we'll  
## creates a  “matrix” object and then cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ## x  is a square invertible matrix
  ## and will be return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list will be pass in input to cacheSolve()
  
  environment = NULL
  set = function(y) {
    # by using '<<-' we can assign a value to an object in environment 
    x <<- y
    environment <<- NULL
  }
  # Note: set() is a function, and part of what it does is to set environment to NULL.
  # But environment is defined outside of set(), that's why we need '<<-'  aslo in set function.
  get = function() x
  setinverse = function(inverse) environment <<- inverse 
  getinverse = function() environment
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

##
## cacheSolve(): in this function we're calculating the inverse of the “matrix” returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed,
## This function retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## x: output of makeCacheMatrix() in our first function.
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inverse = x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(inverse)){
    # get it from the cache and skips the computation. 
    message("Getting cached data")
    return(inverse)
  }

  # otherwise, calculates the inverse 
  matrix.data = x$get()
  inverse = solve(matrix.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(inverse)
  
  return(inverse)
}
