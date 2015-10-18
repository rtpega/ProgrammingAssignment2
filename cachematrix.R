## 

## This function initializes the variables used to know if the 
## inverse of the matrix have been claculated before

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) m <<- solve
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function returns the inverse of the matrix, 
## the first time it is calculated, the second a successive times 
## returns the value stored the first time.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  

  
  m <- x$getinverse()
  
  ## If the inverse of this matrix have been previosly calculated, 
  ## the result  will be stored in m. No more calculus are required
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## In other case, the inverse is calculated 
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
