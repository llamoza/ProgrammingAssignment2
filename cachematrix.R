## This R script has two functions that compute and cache the inverse of a 
## non-singular matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(A = matrix()) {
  IA <- NULL
  set <- function(B) {
    A <<- B
    IA <<- NULL
  }
  get <- function() A
  setinv <- function(inv) IA <<- inv
  getinv <- function() IA
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse (if it exists) of the special "matrix"
## returned by makeCacheMatrix above

cacheSolve <- function(A, ...) {
  ## Return a matrix that is the inverse of 'x'
  IA <- A$getinv()
  if(!is.null(IA)) {
    message("getting cached data")
    return(IA)
  }
  data <- A$get()
  # Determines if the matrix is non-singular
  test <- det(data)
  if(test != 0){
    IA <- solve(data, ...)
    A$setinv(IA)
    IA
  }
  else{
    message("the matrix is singular")
    A$setinv("the matrix has not inverse")
  }
}
