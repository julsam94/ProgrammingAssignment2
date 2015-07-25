# These functions calculates the inverse of a matrix and saves it to the cache.

# makeMatrix Contains the following functions:

# - setm: set the value of the matrix
# - getm: set the value of the matrix
# - setinverse: set the value  of the invese of the matrix
# - getinverse: ger  the value  of the invese of the matrix


makeMatrix<-function(x=matrix()){
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

cacheinverse<-function(x,...){
      m<- x$getinverse() 
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$getm()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
