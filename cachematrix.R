
##This function basically works like the example functions makeVector(). The only 
##difference is that we now have a matrix instead of a vector.
##Here, setinverse sets the inverse of the matrix and getinverse gets the inverse 
##of a matrix from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve is also very similar to the example function cachemean.
## Instead of calculating the mean of a vector it calculates the inverse of a matrix 
## via  the function solve()
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
