## The below functions allows you to calculate the inverse of a squared matrix.
##The main idea is to avoid recalculating inverse matrices, so if once they are calculated,
## the results come from the cache memory.

## makeCacheMatrix sets the initial values for the matrix and inverse, creating function that
##sets and retieve the inverse matrix values.

makeCacheMatrix <- function(x = matrix()) {

  #begins by setting the inverse matrix to NULL that will store a future matrix  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #returns the matrix, x
  get <- function() x
  
  #sets the inverse matrix
  setinv <- function(solve) m <<- solve
  
  #returns the inverse matrix, m
  getinv <- function() m
  
  #returns the 'special vector' containing all of the functions just defined
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## cacheSolve gets the request of retrieving the inverse of a matrix.
## If the value of that inverse matrix exists, then it returns the value of the stored matrix
## from the cache memory. If not, it calculates and stores it.

cacheSolve <- function(x, ...) {

# If the inverse matrix does not exists, then it solves it.
# If it does, the it gets it from the get function or "cache memory"
  m <- x$getinv()
  
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  
  m    
  ## Return a matrix that is the inverse of 'x'
}
