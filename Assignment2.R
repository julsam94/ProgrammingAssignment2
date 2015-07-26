# These functions calculates the inverse of a matrix and saves it to the cache.

# makeCacheMatrix Contains the following functions:

# - setm: set the value of the matrix
# - getm: set the value of the matrix
# - setinverse: set the value  of the invese of the matrix
# - getinverse: ger  the value  of the invese of the matrix


makeCacheMatrix<-function(x=matrix()){
      m <- NULL
      setm <-function(y) {
            x <<- y
            m <<- NULL
      }
      getm<-function() {x}
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() {m}
      list(setm = setm, getm = getm, 
           setinverse = setinverse, getinverse = getinverse)
}

# This function calculates the inverse of the matrix 

cacheSolve<-function(x,...){
      m<- x$getinverse() 
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      dt <- x$getm()
      m <- solve(dt)
      x$setinverse(m)
      m
}

