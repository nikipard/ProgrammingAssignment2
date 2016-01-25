
#makeCacheMatrix creates a special matrix which creates its cached inverse. It also
## has 4 more subfunctions which may set a new matrix (set), get the matrix (get),
## set the inverse matrix (setCacheInverse) and gets the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {   ## x is my special makeCacheMatrix
  my_inverse <- NULL
  set <- function(y) {
    x <<- y                  ##new matrix y
    my_inverse <<- NULL
  }
  get <- function() x       ##shows my matrix
  setCacheInverse <- function(solve) my_inverse <<- solve
  getCacheInverse <- function() my_inverse
  list(set = set, get = get,
       setCacheInverse = setCacheInverse,
       getCacheInverse = getCacheInverse)
}

## The cacheSolve uses the special matrix created above and returns the cached inverse of the matrix.
##If the inverse has not been defined it is estimating the inverse and returns it.
## If the inverse has already been calculated then it returns that inverse.

cacheSolve <- function(x, ...) {   ## using the special "matrix" makeCacheMatrix
  my_inverse <- x$getCacheInverse()
  if(!is.null(my_inverse)) {
    message("getting cached data")
    return(my_inverse)
  }
  data <- x$get()
  my_inverse <- solve(data, ...)
  x$setCacheInverse(my_inverse)
  my_inverse
}