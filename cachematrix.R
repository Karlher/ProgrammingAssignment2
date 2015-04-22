## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The functions does:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3.Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
##retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example,
## if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
# This is a test with an inversible Matrix
x = rbind(c(3,5,6), c(6,5,7),c(7,8,9))
z = makeCacheMatrix(x)
z$get()
cacheSolve(z)
##Results:
##> z$get()
##[,1] [,2] [,3]
##[1,]    3    5    6
##[2,]    6    5    7
##[3,]    7    8    9
##> cacheSolve(z)
##[,1]  [,2]  [,3]
##[1,] -0.55  0.15  0.25
##[2,] -0.25 -0.75  0.75
##[3,]  0.65  0.55 -0.75
