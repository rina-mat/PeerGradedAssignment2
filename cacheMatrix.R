## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix(): creates a special “matrix” object that can cache 
##          its inverse.
## cacheSolve(): computes the inverse of the “matrix” returned by 
##          makeCacheMatrix(). If the inverse has already been calculated and 
##          the matrix has not changed, it’ll retrieves the inverse from the 
##          cache directly.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ##  The following returns a list containing functions to:
  ##      1. set matrix
  ##      2. get matrix
  ##      3. set inverse
  ##      4. get inverse
  
  
  set <- function(y){
    x <<- y
    print(x)
    inv <<- NULL 
  }
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse   #inverse computation
  getinv <- function() inv 
  list(set = set, get = get,
       setinv = setinv ,
       getinv = getinv)
}


## The cacheSolve function returns the inverse of the original matrix input to 
##      makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() 
  ## if the inverse has already been calculated get it from cache and skip the computation
 
  if(!is.null(inv)) {  
    message("getting cached data")
    return(inv) 
  }
  ## else calculate the inverse 
  data <- x$get()
  inv <- solve(data, ...)   #inverse computation

  x$setinv(inv)   ## set the value of the inverse in cache via setinv function.
  inv
}