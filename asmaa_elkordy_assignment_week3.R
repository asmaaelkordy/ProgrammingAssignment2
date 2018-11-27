rm(list = ls())
makeCacheMatrix <- function(ma = matrix()) {
  im <- NULL
  setMatrix <- function(y) {
    ma <<- y
    im <<- NULL
  }
  getMatrix <- function() ma
  setinverse <- function(inv) im <<- inv
  getinverse <- function() im
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if (!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

d<-matrix(c(1,2,3,4),2,2)

d1<-makeCacheMatrix(d)

cacheSolve(d1)

d2<-makeCacheMatrix(-d)

cacheSolve(d1)

cacheSolve(d2)