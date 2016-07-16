## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Method to create an object which stores a matrix and it's cache and functions to change the 
## matrix and access it's cached inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ##variable to hold the matrix cache <- null by default
    set <- function(y){ ##function to change the matrix <- also resets the cached inverse
        x <<- y
        inv <- NULL
    }
    get <- function() x ##function to return the original matrix
    
    ##functions to set and retrieve the cached version of the inverse
    ##the inverse is only set when the cacheSolve function is called
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    #returns list containing the 4 relevant functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

##function to check if a cached inverse exists and then either returns it or
##computes the inverse and caches it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(is.null(i)){
        print("cache not found")
        ##finds the inverse and then stores it if a cached version is not found
        i <- solve(x$get())
        x$setinv(i)
    } else {
        print("cache found")
    }
    i
}
