## Two complimentary functions which provide efficient access to the inverse of a matrix.
##
## If a matrix is created using the makeCacheMatrix function, use the cacheSolve function 
## to efficiently get its inverse. When trying to get the inverse of a matrix
## created with the makeCacheMatrix function, the cacheSolve function first checks to see
## if the inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function.


## Used to create a special object that stores a matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  if(!is.null(i)) {
  
    message("getting cached data")
    
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data)
  
  x$setinverse(i)
  
  i
}
