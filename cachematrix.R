## this function creates a special matrix that can cache its inverse, when called by cacheSolve
## this is programming assignment 2

## Mike Glennon 25-Apr-2015

makeCacheMatrix <- function(x = matrix()) {
      x.inv <- NULL
      # I don't think set is used
      set <- function(y) {
            x <<- y
            x.inv <<- NULL
      }
      get <- function() x                        ## just returns x when get is called. e.g. x$get() returns x
      setinv <- function(inv) x.inv <<- inv      ## function in which "x.inv" takes the cached value of "inv"
      getinv <- function() x.inv                 ## just returns m when get is called. e.g. x$getinv() returns x.inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## This calculates the inverse of the special matrix created above, unless a cache copy exists. in which case it takes that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      x.inv<-x$getinv()
      if(!is.null(x.inv)) {
            messsage("getting cached inverse")
            return(x.inv)
      }
      data<-x$get()
      x.inv<-solve(data)
      x$setinv(x.inv)
      x.inv
}
