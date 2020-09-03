## [Put comments here that describe what your functions do]
##This function consists of get, set, getinverse, setinverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
    
  }
  
  get <- function() x
  setinverse <- function(inverse) inv<<-inverse
  getinverse <- function () inv
  list (set =set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

##This is used to get the cache data. It computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)){
    message ("getting cached data")
    return(inv)
    
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}