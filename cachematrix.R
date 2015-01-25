## The function caches the inverse of a matrix


## makeCacheMatrix set and get a matrix
## then set and get the inverse of the matrix to m

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##return the input matrix
  get <- function() x
  ## set the invert matrix
  setInv <- function(iMatrix) m <<- iMatrix
  ## return th einversed matrix
  getInv <- function() m
  
  ## Return a list of functions
  list(set=set, get=get,
       setInv = setInv,
       getInv = getInv)
  
}

## cacheSolve provides the actual calculation inverse of the matrix and returned
## the saved and cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## get the inversed matrix
  m<- x$getInv()  
   
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## getting the matrix
  data <- x$get()
  ## calculate the inverse matrix
  m<- solve(data, ...)
  ## assign result inversed matrix
  x$setInv(m)
  m
  
}
