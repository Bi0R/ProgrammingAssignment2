## Two functions that are used to create a special object 
##that stores a numeric matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #inv will store the inverse, it is initialized as null
  inv <- NULL
  
  #will store the matrix provided into x (different scope), inv is set to null
  #because if a new matrix is provided the inv must be recalculated
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #returns the matrix previously provided
  get <- function() x
  
  #stores the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  #returns the inverse matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  #gets the inverse value stored in the special vector
  inv <- x$getinverse()
  
  #checks if an inverse value already exists to avoid recalculating
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #If there is no previous value the inverse is calculated and saved for later
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
