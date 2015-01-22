## There are two functions in this file.
## The first function creates a list object containing a matrix and a set of 
## "methods" or functions, which allow the values, the inverse of the matrix, 
## to be either written to or read from.
## The second function takes in the list output from the first function as 
## argument and checks to see if there was a previously stored value in the 
## inverse attribute of the list. If so, this cached value is returned. If not,
## the inverse of the matrix stored in the list is calculated, stored into the
## inverse attribute in the list, and returned to the calling environment.

## This function creates a list containing a matrix and get/set, 
## getinverse/setinverse functions.

makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
	
    set <- function(y) 
	{
	    x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(nvrs) inverse <<- nvrs
    getinverse <- function() inverse
	
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function receives a list created by makeCacheMatrix as input
## checks if a previously calculated inverse of the matrix in the list is
## present. If so return it. If not, calculate it and store it, and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  results <- x$getinverse()
  if(!is.null(results)) {
    message("Getting cached inverse")
    return(results)
  }
  data <- x$get()
  results <- solve(data)
  x$setinverse(results)
  results		
}
