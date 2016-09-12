# Peer Graded Assignment: Programming Assignment 2: Caching the Inverse of a Matrix

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


## Unit Test:
## Now we can write unit test to get diffrence time in inverse matrix that calculated
## and cachesolve that retrieved the inverse from the cache.

testfunction = function(matrix){
  ## @matrix: an invertible matrix
  
  temp = makeCacheMatrix(matrix)
  
  start.time = Sys.time()
  cacheSolve(temp)
  duration = Sys.time() - start.time
  print(duration)
  
  start.time = Sys.time()
  cacheSolve(temp)
  duration = Sys.time() - start.time
  print(duration)
}

# the results 
set.seed(136429)
# creating randome number 
r = rnorm(1000000)
# creating matrix
mat = matrix(r, nrow=1000, ncol=1000)
# passing to testfunction
testfunction(mat)


## The result in my system like this(the results depends on your system!):


# Time difference of 3.468957 secs
# Getting cached data
# Time difference of 0.0003330708 secs

## As you see in the results retrieving from cache ~ 99% decrease results.
## you can test it for large matrix! but be careful when you are creaitng large randome number!



