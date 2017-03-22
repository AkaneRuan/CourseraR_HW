## The whole function caches the inverse of a matrix, while computing the 
##inverse of Matrix is time cosuming.


## makeCacheMatrix is a function that return a list object that can cache the inverse of Matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() computes the inverse of the  "matrix" returned by makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
      message('getting cached data')
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
## sample code to run: myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## cacheSolve(myMatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
