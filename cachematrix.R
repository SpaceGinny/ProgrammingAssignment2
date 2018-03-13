## This script contains two functions, the goal of which is to make a
## faster cached version of the inverse. 
##
## First, in the makeCacheMatrix function, a list of functions are 
## created to get the data and make the inverse. Then in cacheSolve
## the inverse is computed using solve() unless the inverse has 
## already been computed, in which case it takes the computed value


## This function creates a matrix into a matrix object that can 
## cache its inverse to save time
makeCacheMatrix <- function(x = matrix()) {
       # make variable inv
       inv <- NULL
       # set function, makes variables visible in multiple functions
       set <- function(y) {
              x <<- y
              inv <<- NULL
       }
       # gets input
       get <- function() x
       # gets inv to inverse using solve
       setinv <- function(solve) inv <<- solve
       # gets the inverse if necessary
       getinv <- function() inv
       # output
       list(get = get, set = set, 
            getinv = getinv, setinv = setinv)
}


## This function computes the inverse of the matrix object returned by
## makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       # first get the inverse if already computed
       inv <- x$getinv()
       # if the inverse is already computed, just return inverse
       if(!is.null(inv)){
              message("getting cached data for inverse")
              return(inv)
       }
       # get the input data
       data <- x$get()
       # make the inverse
       inv <-solve(data, ...)
       # now set the inverse in special matrix to be inverse for future
       # use
       x$setinv(inv)
       # final value is output of function
       inv
       
}
