## function makeCacheMatrix stores the matrix and its cached inverse (when computed the first time)
## instead of evaluating the inverse for the same matrix everytime, we cahce the result
## faster this way and computationally efficient.

## Write a short comment describing this function
## this environment mimics Object Oriented solution to the Singleton design pattern,
## allows inputting matrix assignment only ussing the set() function and this way we 
## can reset the cache so that the next time cacheSolve() is invoked it computes the 
## inverse for the new matrix and caches the result
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## this function checks first if there is cached inverse for our matrix, 
## if so, it just returns it.  if not, it computes the inverse using Solve()
## and caches it using setinv() for the x object (list).

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    #message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
