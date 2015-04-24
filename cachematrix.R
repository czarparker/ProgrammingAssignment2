## These functions allow the inverse of a matrix to be calculated and
## stored for subsequent use

## makeCacheMatrix is a function containing a set of functions
## that stores a matrix and its inverse for later use using
## the set/get and setinverse/getinverse  functions

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      ## set and get functions for the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      
      ## set and get functions for the inverse of the matrix
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve returns the inverse of a square matrix.
## If the inverse has been previously calculated and cached
## the result will be retrieved from memory without recalculating.
## Otherwise, the calculation will occur, be returned, and cached
## for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      
        ## Check for cached data
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
        ## Calculate inverse
      inv <- solve(x$get(), ...)
      
        ## Cache and return inverse
      x$setinverse(inv)
      inv      
}
