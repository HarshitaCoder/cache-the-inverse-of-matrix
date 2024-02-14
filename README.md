# cache-the-inverse-of-matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  list(set = set, get = get, getinverse = getinv, setinverse = setinv)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
> m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2))
> cacheSolve(m)
     [,1] [,2]
[1,] -2.0  1.5
[2,]  1.0 -0.5
> cacheSolve(m)
getting cached data
     [,1] [,2]
[1,] -2.0  1.5
[2,]  1.0 -0.5
