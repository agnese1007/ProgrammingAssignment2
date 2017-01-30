## MakeCacheMatrix creates a cache matrix of the original matrix.
## it contains 4 founctions:
## 1) set the value of the atrix
## 2) get the value of the matrix
## setinverse set the inverse value of the matrix
## getinverse get the inverse value of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ## initialize the value of inverse matrix
      m <- NULL
      ## create the matrix that is to be inversed and cached
      set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
      } 
      get <- function() x
      ## calcualte the inverse of the matrix
      setinverse <- function(solve) m <<- solve
      ## get the inverse of the matrix
      getinverse <- function () m
      ##passes the values to the function makeCacheMatrix
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse matrix cached with makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## if inverse exists, it is retrieved
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
          return(m)
        }
        ## if inverse doesn't exist it is calcualted and retrieved
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
