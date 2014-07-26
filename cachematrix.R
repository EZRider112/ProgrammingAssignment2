makeCacheMatrix <- function(x = integer()) {
  ## This function creates a special "matrix" object that can cache its inverse
  ## The function requires the user to enter a vector of integers and creates the matrix from those integers
  
  ## an n-by-n square matrix A is called invertible if there exists an n-by-n square matrix B such that...
  ## AB = BA = I(n)
  ## A matrix must be a square (same number of rows as columns) to be invertible however not all square...
  ## matrices are invertible
  ## This function assumes an invertible square matrix
  
  ## This line of code calculates the number of rows/columns for our square matrix
  ## If required an error check would have been put into place to ensure the list of integers...
  ## could be converted to a square matrix
  cnt <- sqrt(length(x))
  ## This line of code creates out matrix from x
  MX <- matrix(x, cnt, cnt)
  
  m <- NULL
  
  ## This function sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## This function gets the value of the matrix
  get <- function() x
  
  #This function sets the value of the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  
  #This function gets the value of the inverse of the matrix
  getinverse <- function() m
  
  # This line of code contructs our list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
  ## should retrieve the inverse from the cache.
  
  ## Check to see if the cashed inverse already exists
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  # Calculate the inverse of the matrix and sets the value of the matrix in the cache via a setinverse function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
