## makeCacheMatrix creates a matrix which is actually a list with function that: set and get 
## value of a matrix;and also to set and get value of a mean. The set function assigns the value
## assigns the value of y to x, while the get function gets the value of x. The setinverse 
## function sets the inverse, and getinverse function get the inverse to the console

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){ 
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## cacheSolve computes the inverse of the matrix created with makeCacheMatrix function. A check
## to determine whetter inverse has been computed si done using the argument in the if function. If
## so, gets inverse value from cache, if otherwise, a new inverse value set and re-computed.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


