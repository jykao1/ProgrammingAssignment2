## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCasheMatrix function will first initial the x as a matrix type and 
## set m to NULL (m is the inverse of matrix)
## 4 basic sub-functions has been defined as set, get, setinverse, 
## and getinverse.  makeCasheMatrix will return a list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve function will first check if m (inverse of matrix) is Null
## or not.  Since makeCacheMatrix() sets the cached inverse to NULL 
## whenever a new vector is set into the object, if the value here is 
## not equal to NULL, we have a valid, cached inverse and can return it 
## to the parent environment. After validation, the function will then 
## inverse the matrix and return the data.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cashed data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

## sample test on Inverse functions:
## > a <- matrix(1:4, nrow=2, ncol=2)
## > a
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > test <- makeCacheMatrix(a)
## > cacheSolve(test)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## ------------------------the end ---------------------