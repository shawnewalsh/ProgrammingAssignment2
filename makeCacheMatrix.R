makeCacheMatrix <- function(x = matrix()) {

  invMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  

  get <- function() x
  
  setinverseMatrix <- function(invMatrixerse) invMatrix <<- invMatrixerse
  getinverseMatrix <- function() invMatrix
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get,
       setinverseMatrix = setinverseMatrix,
       getinverseMatrix = getinverseMatrix)
}


cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverseMatrix()

  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }

  data <- x$get()
  invMatrix <- solve(data, ...)

  x$setinverseMatrix(invMatrix)

  invMatrix
}
