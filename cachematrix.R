## This function will calculate the inverse of a matrix and  
## store it, and if the user tries to submit the same matrix
## for the inverse calcluation, the function will return the
## stored value.

## makeCache returns a list of functions and will store
## a matrix and the cached value of the inverse of the matrix.
## The following list of functions are identfied:
## a. setMatrix  ==> sets the value of the matrix
## b. getMatrix  ==> gets the value of the matrix
## c. setInverse ==> sets the inverse of the matrix as the cached value
## d. getInverse ==> returns the cached (inverse matrix) value

makeCache <- function(x = matrix()) {
    ## defines and sets cache to NULL
  cache <- NULL
    ## a. setMatrix - stores the matrix 
  setMatrix <- function(value) {
      ## within the parent environment, assigns x to the value
    x <<- value
      ## within the parent environment, re-sets the cache to NULL
    cache <<- NULL
  }
    ## b. getMatrix - return the value of the matrix x
  getMatrix <- function() x 
    ## c. setInverse - sets the cache equal to the inverse of the matrix x
  setInverse <- function(inverse) cache <<- inverse
    ## d. getInverse - returns the cached inverse of x
  getInverse <- function() cache 
    #returns a list; each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = 
         cacheInverse, getInverse = getInverse)
}


## This function calculates the inverse of the matrix provdied by the user
## The function first checks to see if the inverse has already been 
## calculated. If it has, then the function will return the cached value
## saving the computation time. If it has not, the the inverse if calculated
## cached

cacheSolve <- function(x, ...) {
    ## retrievest the cached value fo the matrix of x
  inverse <- x$getInverse()
    ## if a cached value exists, the value is returned
  if(!is.null(inverse)) {
    message("retrieving cached data")
    return(inverse)
  }
    ## if a cached value doesnot exist, the inverse is calculated and stored
  data <- x$getMatrix()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
    ##returns calculated inverse
  inverse
}
