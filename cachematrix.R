## makeCacheMatrix creates a special matrix object then providing access to a cached copy of the matrix's inverse.
## If the matrix inverse has already been calculated, it will then find it in the cache and return it, and not calculate it again.



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newInv) inv <<- newInv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




## Returns a matrix that is the inverse of the matrix stored in x.
## Gets the returned matrix from cache otherwise calculates it and caches it for future calls.


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
