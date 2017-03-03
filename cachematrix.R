## The following function group, which contain two major functions, is used to cache the Matrix inversion,
## while the first function produces a special matrix and the second matrix gives its inverse

## The following function makeCacheMatrix is used to give a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	 s<- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function cacheSolve is used to calculate the inverse of the matrix from the above function.
## If the inverse has been calculated, this function just retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
