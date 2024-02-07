## Put comments here that give an overall description of what your
## functions do

## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #Set the inverse matrix to NULL
  inverse <- NULL
  
  #set function: allow to change the matrix we want to invert
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #get function to get the matrix
  get <- function() x
  #setter to the inverse matrix
  set_inverse <- function(inv) inverse <<- inverse
  #getter to the inverse matrix
  get_inverse <- function() inverse
  
  #Create an object with this functions
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## Function to compute the inverse of the matrix in makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  #Check if an inverse already exists
  inverse <- x$get_inverse()
  
  #If inverse exists, return it
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  
  ## If not, return a matrix that is the inverse of 'x'
  new_matrix <- x$get()
  inverse <- solve(new_matrix)
  inverse
}
