# The function makeCacheMatrix takes a matrix as argument and creates a
# special 'matrix' which infact is a list containing a function that will
# a) set the value of the matrix
# b) get the value of the matrix
# c) set the value of the inverse of the argument matrix
# d) get the value of the inverse of the argument matrix

makeCacheMatrix <- function(x = matrix()) {
#==========================================================================
# setting 'i' to NULL as placeholder for its future values
  i <<- NULL
#==========================================================================
# Setting the matrix x, to a new matrix, y, and resets 'i' to NULL  
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
#==========================================================================
# Returning the set matrix x
  get <- function() x
#==========================================================================
# Setting the value of 'i' to inverse
  setinverse <- function(inverse) i <<- inverse
#==========================================================================
# Getting the value of 'i' that was set 
  getinverse <- function() i
#==========================================================================
# Returning the list by the main function makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

# ---------------------------------------------------------------------------------------------------------------------------------

# The cacheSolve function takes the output of makeCacheMatrix as argument
# and returns the inverse of the argumet matrix to the makeCacheMatrix function.
# It will not calculate inverse if it has been once calculated for a given 
# argument and only return the previously calculated or cached value.

cacheSolve <- function(x, ...) {
  
# Storing the present value of inverse (latest return by makeCacheMatrix) at 'i'
  i <- x$getinverse()
#================================================================================
# if 'i' is not NULL, then cached or previously calculated value of 'i' 
# will be returned
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
#================================================================================ 
# Otherwise(i.e. if 'i' is NULL), calculate the inverse for the first time
# with the solve function and return
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
