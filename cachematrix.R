## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix takes matrix as input argument and creates a list of functions about that input argument matrix
## It is assumed that the input matrix is inversible
makeCacheMatrix <- function(x = matrix()) {
  ## start by setting the cached value to NULL.
  x_inv <- NULL
  
  ## creating the set() function that initializes the input matrix and sets the inverse value to NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  ## creating the get() function that just returns the input matrix
  get <- function() x
  
  ## creating the set_inv() function that can set the value for the inverse 
  set_inv <- function(inv) x_inv <<- inv
  
  ## creating the get_inv() function that just returns the value of the inverse
  get_inv <- function() x_inv
  
  ## return a list of all the functions created
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)

}

## cacheSolve returns and prints the inverse of an input matrix. 
## The input expected is list of functions from makeCacheMatrix, so that cachSolve can use these functions to either calculate the inverse
## or retrieve an already calculated and cached version of the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## retreiving cached value for the inverse using makeCachedMatrix's function get_inv()
  x_inv <- x$get_inv()
  
  ## if there is a cached value, return that and print an appropriate message
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  ## otherwise (if there is no cached inverse) get the actual input matrix using get() from makeCacheMatrix
  data <- x$get()
  
  ## calculate it's inverse at this point
  x_inv <- solve(data)
  
  ## set the cached value to the just calculated value for the inverse
  x$set_inv(x_inv)
  
  ## return the inverse
  x_inv
  
  
}


## Execution example
# a <- matrix(c(1,2,3,3,4,4,5,5,5),nrow=3,ncol=3)
# df <- makeCacheMatrix(a)
# df$get()
# df$get_inv() #this returns null
# cacheSolve(df) #this calculates inv ,stores and prints it
# df$get_inv() # now inv is not null, but the previous calculated value
# cacheSolve(df) # now 

