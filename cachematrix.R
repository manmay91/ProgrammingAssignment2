## Caching the inverse of a matrix
## Inversion of matrix is generally a costly computation so it is more convenient
## to cache inverse of the matrix rather than computing it repeatedly
## The functions given in this program are used to create special object
## that stores a matrix and caches its inverse

## This function provides a special matrix object that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of special matrix and if the matrix is already
## calculated, than it should retrive the inverse of the matrix from cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
