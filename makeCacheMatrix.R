# define my function here
makeCacheMatrix <- function(x = matrix()) {
# create an empty object
  invMatrix <- NULL
  
  set <- function(y) {
    # cache x as y
    x <<- y
    invMatrix <<- NULL
  }
  
# get function
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
