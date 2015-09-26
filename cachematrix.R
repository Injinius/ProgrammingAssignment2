## Functions that aid in caching a matrix and its inverse

## makeCacheMatrix
## makes and returns an object that:
## has a set/get functions to set/get the provided matrix
## has setinverse/getinverse functions to return the matrix's inverse
##

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse 
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## Returns the inverse (via solve()) of the provided matrix.
## If the inverse was previously cached, then return the cached value.
##

cacheSolve <- function(x, ...) {
  ## Assumption: x is invertible
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

##
## Sample run
##
# > bbb<-matrix(c(1, 3, 3, 1, 4, 3, 1, 3, 4), 3, 3, byrow=TRUE)
# > cbbb <- makeCacheMatrix(bbb)
# > cbbb$get()
#      [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4
#
# > cacheSolve(cbbb)
#     [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
#
# > cacheSolve(cbbb)
# getting cached data
#      [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
