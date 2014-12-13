# Creates a special "matrix"to set the value of the matrix, 
# get the value of the matrix and set the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y                         # x in the containing environment is initialized
        m <<- NULL                      # m in the containing environment is initialized
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse  
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cachesolve calculates the Inverse of the special "matrix" 
# created with the above function. 


cacheSolve <- function(x, ...) {
    
    # Checks to see if the Inverse has already been calculated. 
    # If so, it gets the inverse from the cache and skips the computation. 
        
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")  # since local variable 'm' exists within global there is no need to search the containing
        return(m)
    }
    
    # Calculates the inverse of the data and sets the value of the 
    # Inverse in the cache via the setinverse function.
    
    data <- x$get()
    m <- solve(data, ...)               # calculate inverse via solve
    x$setinverse(m)
    m
}
  
