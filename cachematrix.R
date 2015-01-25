## The function caches the inverse of a matrix


## makeCacheMatrix set and get a matrix
## then set and get the inverse of the matrix to m

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInv <- function(iMatrix) m <<- iMatrix
  getInv <- function() m
  list(get=get,
       setInv = setInv,
       getInv = getInv)
  
}

## cacheSolve provides the actual calculation inverse of the matrix and returned
## the saved and cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setInv(m)
  m
  
}
