## Functions for caching the inverse of function

## Write a short comment describing this function
## Function returns a special 'matrix' object with attributes for
## optimizing getting the inverse of matrix by keeping a cache of it.
## The matrix also includes methods for updating and reporting the matrix
## and it's inverse.
makeCacheMatrix <- function(x = matrix()){
  x_invrs <- NULL
  invrs_updated <- TRUE
  set <- function(y){
    x <<- y
    x_invrs <<- NULL
    invrs_updated <<- FALSE
  }
  get <- function() x
  set_invrs <- function(invrs) x_invrs <<- invrs
  get_invrs <- function() x_invrs
  set_status <- function() invrs_updated <<- TRUE
  get_status <- function() invrs_updated
  
  list(set=set, get=get,
       set_invrs=set_invrs,
       get_invrs=get_invrs,
       get_status=get_status)
}


## Write a short comment describing this function
## Function returns inverse of matrix `x` either through retrieving
## cached version or calculating (and caching) it.
cacheSolve <- function(x, ...) {
  test_invrs <- x$get_invrs()
  status <- x$get_status()
  if(!is.null(test_invrs) & status==TRUE){
    message("getting cached data")
    return(test_invrs)
  }
  mtrx <- x$get()
  mtrx_invrs <- solve(mtrx)
  x$set_invrs(mtrx_invrs)
  x$set_status()
  return(mtrx_invrs)
}
