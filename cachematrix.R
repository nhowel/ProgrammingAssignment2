## These functions enable the inverse of a matrix 
## to be cached rather than calculated each time. 

## makeCacheMatrix, creates a special 
## matrix that caches its inverse. It inputs a matrix 
## and returns a list containing four functions: 
## set, get, setinverse, and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get <- function () x
  setinverse <- function(inverse) m<<-inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of the matrix
## returned with makeCacheMatrix. If it
## has already been solved an has not been changed, 
## it returns the cached value. 

cacheSolve <- function(x = matrix()) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setinverse(m)
  m
}
