## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix creates a special matrix capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #sets x matrix variable and clears inverse cache only if argument is different from original matrix
  set <- function(y) {
    if (!identical(x,y)) {
      x <<- y
      inv <<- NULL      
    }
  }
  
  #returns x matrix variable
  get <- function() x
  
  #sets inverse cache variable
  setinverse <- function(inverse) inv <<- inverse
  
  #returns inverse cache variable
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve returns the inverse of a matrix (cache will be returned if available)

cacheSolve <- function(x, ...) {
  #return inverse cache if available
  inv <- x$getinverse()  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #if cache not available, calculate inverse and set the inverse cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  #return inverse matrix
  inv
}
