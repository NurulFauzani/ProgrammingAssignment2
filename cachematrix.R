# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix.


# makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the inverse property
  m <- NULL
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Method the get the matrix
  get <- function() 
  # Return the matrix
    x
  # Method to set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  # Method to set the inverse of the matrix
  getinverse <- function()
  # Return the inverse property  
    m
  # Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# The following function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via 
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'     
  m <- x$getinverse()
  # Just return the inverse if its already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Get the matrix from our object
  data <- x$get()
  m <- solve(data, ...)
  ## Set the inverse to the object
  x$setinverse(m)
  ## Return the matrix
  m
}


## Sample run:

# m<- makeCacheMatrix(matrix(c(1,2,4,7),2,2))

# cacheSolve(m)     
#>        [,1] [,2]
#>  [1,]   -7    4
#>  [2,]    2   -1     

# cacheSolve(m) 
#> getting cached data
#>        [,1] [,2]
#>  [1,]   -7    4
#>  [2,]    2   -1