## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # The first function, makeCacheMatrix creates a special "matrix",
  ## object that can cache its inverse.
  
  inv <- NULL
  
  # 'inv' will be "our inverse of matrix" and it's reset to NULL every
  # time makeCacheMatrix is called
  
  # note these next three functions are defined but not run
  # when makeCacheMatrix is called.
  
  
  # 1. set the value of the matrix
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  
  # 2. get the value of the matrix
  # this function returns the value of the original matrix
  
  get <- function() x
  
  ## This actually run the "get()" function, and show that the data
  ## have been kept in the cache, as I can call it outside the function
  
  
  # 3. set the value of the inverse of the matrix
  
  setinverse <- function(inverse) inv <<- inverse
  
  # this is called by cacheSolve() during the first cacheSolve()
  # access and it will store the value using superassignment
  
  
  # 4. get the value of the inverse of the matrix
  
  getinverse <- function() inv
  
  # this will return the cached value to cacheSolve() on
  # subsequent accesses
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  # the list is accessed each time makeCacheMatrix() is called,
  # that is, each time we make a new object. This is a list of
  # the internal functions ('methods') so a calling function
  # knows how to access those methods.
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    ## This function computes the inverse of the special "matrix" 
    ## returned by makeCacheMatrix above
    
    ## accesses the object 'x' our special matrix and gets the 
    ## value of the inverse of the matrix
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
      ## If the inverse has already been calculated 
      ## (and the matrix has not changed), 
      
      # ... send this message to the console
      
      message("getting cached data")
      
      ## then the cachesolve should retrieve the inverse from the cache
      ## . ... "return" ends, 
      
      return(inv)
    }
    
    # we reach this code only if x$getinverse() returned NULL
    # if inv was NULL then we have to calculate the inverse matrix
    # store the calculated inverse matrix value in x
    # (see setinverse() in makeCacheMatrix)
    # return the mean to the code that called this function
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    # ... and return the inverse matrix ... "return" ends
    # the function cacheSolve(), 
    
    return(inv)
  }