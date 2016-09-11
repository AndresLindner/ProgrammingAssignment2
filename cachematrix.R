## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

## This  function creates a special ???matrix??? object, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value for the inverse of the matrix
## get the value for the inverse of the matrix
## It is actually a "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of a matrix. In case the inverse has already been calculated,
## then the function would retrieve the cached inverse calculation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
