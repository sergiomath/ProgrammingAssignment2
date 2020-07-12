## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix taking as an example, how to create a vector in the cache
# i create a funtion to make a matrix  with a list to use for :
##1)set a matrix 2)get a matrix 3)set the inversa 4) get the inverse 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

##cacheSolver take a object create for makeCacheMatrix ,first check if the inverse
## is in the cache if not , its calculate the inverse and keep in the cache
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
