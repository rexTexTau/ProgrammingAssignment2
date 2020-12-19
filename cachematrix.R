# Function used to handle matrix inversion in a cached manner
# Returns list of methods:
# * set - to set the matrix value
# * get - to receive previously stored matrix value
# * setinverse - to set the matrix inverse value 
# * getinverse - to get previously stored 
# Usage sample:
# cm <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
# cacheSolve(cm)
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  set <- function(matrixvalue) {
    x <<- matrixvalue
    # The matrix has been changed, so the previously 
    # calculated inverse is not valid at the moment
    cache <<- NULL 
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inversevalue) {
    cache <<- inversevalue
  }
    
  getinverse <- function() {
    cache
  } 
  
  # public function list
  list(set = set, 
       get = get,
       setinverse = setinverse, 
       getinverse = getinverse
  )
}


# Function to get the matrix inversion
# First it tries to get value from cache
# If not succeeded, actual inverse calculated and cached
# Return a matrix that is the inverse of 'x'
# Usage sample:
# cm <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
# cacheSolve(cm)
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(is.null(i)) {
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
  }
  i
}