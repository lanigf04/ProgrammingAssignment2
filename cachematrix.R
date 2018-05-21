## The following functions create a special "matrix" and computes its inverse


## This function creates a special "matrix"object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function() i <<- solve(x)
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This 2nd function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse()
  i
  
}
## To test
M<-matrix(c(1,2,3,4),2,2)
M1<-makeCacheMatrix(M)
cacheSolve(M1)
       [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5