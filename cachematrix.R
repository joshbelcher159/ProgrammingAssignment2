## The main purpose of the first function, makeCacheMatrix, is to create
## a matrix object, m, which can be cached. The second function returns
## the inverse of the original matrix (put through makeCacheMatrix), either
## by computing it if it has not already been computed, or by returning
## the cached value.

## Creates a matrix object m that can be cached. Also creates functions
## which will be used to set, and retrieve the cached matrix. The list is
## used to make the functions within makeCachematrix visible outside of their
## assigned environment.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(solve) m <<- solve
  getMatrixInverse <- function() m
  list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}


## If m (the matrix object which stores the cached inverse) is empty, cacheSolve
## computes the inverse of the matrix returned by makeCacheMatrix, and caches it.
## If the cache already exists then the function simply returns that value.
cacheSolve <- function(x, ...) {
  m <- x$getMatrixInverse()
  if(is.null(m)) {
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrixInverse(m)
    m
  } else {
    return(m)
  }
}

