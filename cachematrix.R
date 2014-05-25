
## It should inverse the representation of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  
  set <- function(newMatrix)
  {
    x <<- newMatrix
    inversedMatrix <<- NULL
  }
  
  get <- function() x
  setinversed <- function(inv) inversedMatrix <<- inv
  getinversed <- function() inversedMatrix
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed = getinversed)
}



## It should return inversed matrix of x

cacheSolve <- function(x, ...) {
  inv <- x$getinversed()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinversed(inv)
  inv
}
