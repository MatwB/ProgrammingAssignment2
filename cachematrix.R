##----------------------------------------------------------------------
##     Programming Assignment 2
##     CACHING THE INVERSE OF A MATRIX
##----------------------------------------------------------------------
# Solving the inverse of a matrix is usually a time consuming operation
# especially if the matrix has big dimensions; the purpose of this assignment is 
# to write a package of two functions that cache the inverse of a matrix 
# and retrieve that value later if needed. 

# Here are defined two functions 'makeCacheMatrix' and 'cacheSolve'
# 'makeCacheMatrix' 
# receives in input a square matrix which is assumed to be invertible;
# produces in output a list of four internal functions
# (set, get, setinverse and getinverse) associated to the object CacheMatrix.
# 'cacheSolve'
# receives in input a matrix and produces in output the inverse of that matrix;
# in the execution, the function checks if the inverse matrix is already computed;
# if not, it computes the inverse.

##--------------------------------------------------------------------------
## Short comment describing this function:
## 'makeCacheMatrix' creates a special matrix (in reality a list) equipped with 4 functions:
## get/set the original matrix;
## get/set the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL                          #set to NULL the value of the inverse matrix
  set <- function(y) {                  #define the function that set the value of the matrix to be inverted
    x <<- y                                    #set the new value of the matrix to be inverted
    minv <<- NULL                              #set to NULL the value of the cached inverse matrix	
  }
  get <- function() x                   #define the function that get the value of the matrix to be inverted	
  setinv <- function(inv) minv <<- inv  #define the function that set the value of the cached inverse matrix
  getinv <- function() minv             #define the function that get the value of the cached inverse matrix
  list(set = set, get = get,            #return a list with the four functions previously defined
       setinv = setinv,
       getinv = getinv)
}

##---------------------------------------------------------------------------
## Short comment describing this function:
## 'cacheSolve' returns a matrix that is the inverse of 'x':
## if the value of the inverse matrix is already cached, recovers that value;
## otherwise computes the new value.

cacheSolve <- function(x, ...) {
  minv <- x$getinv()                   #get the cached inverse matrix  
  if(!is.null(minv)) {                 #if the cached inverse matrix is already calculated
    message("getting cached data")            #advise recovering value
    return(minv)                              #return the cached value
  }
  message("computing inverse matrix")  #if no cached value, advise computing
  data <- x$get()                             #get the starting matrix (to be inverted) and put into 'data'
  minv <- solve(data, ...)             #calculate the inverse matrix
  x$setinv(minv)                       #store the value of the inverse matrix in the global variable 
  minv                                 #print the value of the inverse matrix
}




