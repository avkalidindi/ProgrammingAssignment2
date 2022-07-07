## The following two function create a 'special' matrix which
## stores/caches its inverse so that the cache version is returned
## when the matrix hasn't changed and avoids repeated computation.

## This function stores the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  
  set <- function(y){
    x <<- y
    mat_inv <<- solve(x)
  }
  
  get <- function() x
  
  set_inverse <- function(inverse){
    mat_inv <<- inverse
  }
  
  get_inverse <- function() mat_inv
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Checks if the inverse of x is already cached
## and if yes, then returns that cache inverse else computes the inverse
## and caches it for future use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$get_inverse()
    
    if(!is.null(inv_mat)){
      print("Returning Cached Inverse")
      return(inv_mat)
    }
    
    mat <- x$get()
    inv_mat <- solve(mat)
    x$set_inverse(inv_mat)
    inv_mat
}
