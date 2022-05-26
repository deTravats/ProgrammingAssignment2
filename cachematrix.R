## This is a pair of function that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inverse <- function(solve) inv <<- solve
  get.inverse <- function() inv
  list(set = set, get = get, 
       set.inverse = set.inverse, 
       get.inverse = get.inverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$get.inverse()
  if(!is.null(inv)){
    message("I have the answer in the cache !")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set.inverse(inv)
        ## Return a matrix that is the inverse of 'x'
  inv
}
