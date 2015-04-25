## this is programming assignment 2
## these two functons form a pair, which create a special matrix which can cache its inverse.
## and create the inverse of the function
## this function creates a special matrix that can cache its inverse, when called by cacheSolve


## Mike Glennon 25-Apr-2015

makeCacheMatrix <- function(x = matrix()) {
      x.inv <- NULL
      # I don't think set is used, but still included here as it is part of the list vector
      set <- function(y) {
            x <<- y
            x.inv <<- NULL
      }
      get <- function() x                        ## just returns x when get is called. e.g. x$get() returns x
      setinv <- function(inv) x.inv <<- inv      ## function in which "x.inv" takes the cached value of "inv"
      getinv <- function() x.inv                 ## just returns m when get is called. e.g. x$getinv() returns x.inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)                     # outputs the list of functions created as a list. these are called using
                                                # x$get(), x$set(new value) etc.
}

## This function calculates the inverse of the special matrix created above, 
## unless a cache copy exists. in which case it takes that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      x.inv<-x$getinv()                         ## uses getinv to returned the cached version
      if(!is.null(x.inv)) {                     ## if x.inv exists then use it, otherwise cal new one
            messsage("getting cached inverse")
            return(x.inv)                       ## exit the function, returning the cached inverse
      }
      data<-x$get()                             ## get the new matrix if the cached copy doesn;'t exist
      x.inv<-solve(data)                        ## use solve to calculate the inverse. Assume is invertable
      x$setinv(x.inv)                           ## write the new version to the cache using setinv
      x.inv                                     ## exit with the value of the inverted matrix
}
