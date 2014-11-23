## Since Matrix inversion is a costly computation, the goal of this pair of functions is
## to caching the inverse of a matrix rather than computing it repeatedly.

## This function creates a special R object that provides the variables and functions
## needed to hold the original matrix and the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setInverseMatrix <- function(InverseMatrix) m <<- InverseMatrix
  getInverseMatrix <- function() m
  
  # return a list of functions as an R object
  list(get=get, 
       setInverseMatrix=setInverseMatrix, 
       getInverseMatrix=getInverseMatrix)
  
}


## This function inverts matrix x, checking first if it has been cached. 
## If not, the inverse of x is calculate, cached & returned.

cacheSolve <- function(x, ...) {
  m <- x$getInverseMatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() 
  m <- solve(data)
  x$setInverseMatrix(m) 
  m
  
}
