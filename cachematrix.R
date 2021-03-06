## Put comments here that give an overall description of what your
## functions do
## CacheMatrix.r
## Functionality include 2 items to cache, calculate and retrieve/set the inverse of given matrix: 
## 1 - makeCacheMatrix() 
## 2 - cacheSolve()

## Write a short comment describing this function
## Caching the Matrix to set and get it's inverse upon command after checking for NULL

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<- function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setinverse<- function(inverse) i<<-inverse
  getinverse <-function() i
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Caching to retrieve the cached matrix in order to provide the inverse, then setting it. If the cached inverse is not null, message is presented followed by the cached inverse.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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

## Testing the code
## This creates and displays the Matrix
x<-matrix(1:4,nrow=2,ncol=2)
print(x)
## This creates a Cached Matrix and displays the environments
xx<-makeCacheMatrix(x)
print(xx)
## This creates the Inverse from the Cache
cacheSolve (xx)
## This retrieves the already generated Cache Inverse
cacheSolve (xx)
