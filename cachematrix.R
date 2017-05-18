## Overall description of functions: Pair of functions that cache the inverse of a matrix


## Makecachematrix: Creates a special "matrix"object that can cache its inverse
## 1.  set the values of the matrix
## 2.  get the values of the matrix
## 3.  set the Inverse
## 4.  get the Inverse

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## Cachesolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (ad the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv

}
