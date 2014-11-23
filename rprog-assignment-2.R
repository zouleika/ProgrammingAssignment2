# The first function, makeVector creates a special "vector", 
# which is really a list containing a function to

makeVector <- function(x = numeric()) {
        m <- NULL
        # m will be "our mean" and it's reset to NULL every
        # time makeVector is called
        # note these next three functions are defined but not run 
        #when makeVector is called.
        
        # 1. set the value of the vector
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # 2. get the value of the vector 
        # this function returns the value of the original vector        
        
        get <- function() x
        
        ## This actually run the "get()" function, and show that the data 
        ## have been kept in the cache,as I can call it outside the function
        
        # 3. set the value of the mean
        setmean <- function(mean) m <<- mean
        
        # this is called by cachemean() during the first cachemean()
        # access and it will store the value using superassignment
        
        # 4. get the value of the mean
        getmean <- function() m
        
        # this will return the cached value to cachemean() on
        # subsequent accesses
        
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
        
        #  the list is accessed each time makeVector() is called,
        # that is, each time we make a new object. This is a list of
        # the internal functions ('methods') so a calling function
        # knows how to access those methods.      
        
}

# The following function calculates the mean of the special "vector" 
# created with the above function

# the input x is an object created by makeVector

cachemean <- function(x, ...) {
        # accesses the object 'x' and gets the value of the mean
        
        m <- x$getmean()
        
        if(!is.null(m)) {
                
                # if mean was already cached (not NULL) ...
                # ... send this message to the console
                
                message("getting cached data")
                
                # ... and return the mean ... "return" ends
                # the function cachemean(), note
                return(m)
        }
        
        # we reach this code only if x$getmean() returned NULL
        # if m was NULL then we have to calculate the mean
        # store the calculated mean value in x (see setmean() in makeVector
        # return the mean to the code that called this function
        
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse

# The second function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # m will be our 'inverse matrix' and it's reset to NULL every
        # time makeCacheMatrix is called
        # note these next three functions are defined but not run 
        #when makeVector is called.
        
        # 1. set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # 2. get the value of the matrix
        # this function returns the value of the original matrix       
        
        get <- function() x
        
        ## This actually run the "get()" function, and show that the data 
        ## have been kept in the cache,as I can call it outside the function
        
        # 3. set the value of the inverse matrix
        setinverse <- function(inverse) m <<- inverse
        
        # this is called by makeCacheMatrix() during the first makeCacheMatrix()
        # access and it will store the value using superassignment
        
        # 4. get the value of the inverse matrix
        getinverse <- function() m
        
        # this will return the cached value to cachesolve() on
        # subsequent accesses
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        #  the list is accessed each time makeCacheMatrix() is called,
        # that is, each time we make a new object. This is a list of
        # the internal functions ('methods') so a calling function
        # knows how to access those methods.      
        
}

# The following function calculates the mean of the special "matrix" 
# created with the above function

# the input x is an object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        # accesses the object 'x' and gets the value of the mean
        
        m <- x$getinverse()
        
        if(!is.null(m)) {
                
                # if mean was already cached (not NULL) ...
                # ... send this message to the console
                
                message("getting cached data")
                
                # ... and return the mean ... "return" ends
                # the function cachemean(), note
                return(m)
        }
        
        # we reach this code only if x$getmean() returned NULL
        # if m was NULL then we have to calculate the mean
        # store the calculated mean value in x (see setmean() in makeVector
        # return the mean to the code that called this function
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}