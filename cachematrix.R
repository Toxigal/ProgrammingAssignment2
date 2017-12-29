##This pair of functions will cache the inverse of a matrix

## The first step is to create the matrix cache

makeCacheMatrix <- function(x = matrix()) {  ## The input into this function will be a matrix
  inv <- NULL                                 ## Create an empty object to be used by the functio
  set <- function(y){                         
    x <<- y                                   ## Assign value of y to x in the parent environment
    inv <<- NULL                              ## Clear value of inv from previous use of CacheSolve
  }
  get <- function() x                         ## Retrieve x from parent environment
  set_inv <- function(solveMatrix) inv_matrix <<- inv  ##Createfunction to solve inverse and assign value of inv to inv_matrix in parent environment
  get_inv <- function() inv                            ##Retrieve inv from parent environment
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)  ##assign functions to list in parent environment
}

## This function retrieves the inverse matrix from makeCacheMarix()

cacheSolve <- function(x, ...) {    ## input is x from parent environment    

  inv <- x$get_inv()                ## Runs get_inv function on x
  if(!is.null(inv)){                ## if result is not null (set to null by MakeCacheVector when there is no valid cache)
    message("getting cached data")  ## Uses cached data
    return(inv)
  }
  inverse_matrix<- x$get()          ##assign matrix from input to inverse_matrix
  inv <- solve(inverse_matrix)      ## calculate inverse of matrix and assign to inv
  x$set_inv(inv)                    ##return result to parent environment
  inv      
}