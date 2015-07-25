makeMatrix<-function(x=matrix()){
      m <- NULL
      set <-function(y) {
            x <<- y
            m <<- NULL
      }
      get<-function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
}

cacheinverse<-function(x,...){
      m<- x$getinverse() 
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
