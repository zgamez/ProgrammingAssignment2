# R function is able to cache potentially time-consuming computations.
# take advantage of the scoping rules of the R language and how they can 
# be manipulated to preserve state inside of an R object.

# makeVector creates a special "vector", which is really a list containing a function to
# set the value of the vector, get the value of the vector
# set the value of the mean and the value of the mean


makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse  # calculate inverse here via solve ?
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The following function calculates the Inverse of the special "matrix" 
# created with the above function. 
# However, it first checks to see if the Inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the 
# Inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) # calculate inverse via solve
    x$setinverse(m)
    m
}
