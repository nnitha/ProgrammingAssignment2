# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix. 

## makeCacheMatrix is a function to 
# 1. set the value of the matrix 
# 2. get the value of the matrix 
# 3. set the value of inverse of the matrix 
# 4. get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL # sets the value of inv to NULL (provides a default if cacheSolve has not yet been used)
  	set <- function(y) {  #set the value of the matrix
        	x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
        	inv <<- NULL # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
     } 
     get <- function() x 
     setinverse <- function(inverse) inv <<- inverse 
     getinverse <- function() inv 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # creates a list to house the four functions

}


# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If so, it gets the result from cache and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via setinverse function. 
  
# This function assumes that the matrix is always invertible. 
 cacheSolve <- function(x, ...) { 
     inv <- x$getinverse() # if an inverse has already been calculated this gets it
     if(!is.null(inv)) { # check to see if cacheSolve has been run before
         message("getting cached data.") 
         return(inv) 
     } 
     data <- x$get() 
     inv <- solve(data) 
     x$setinverse(inv) 
     inv 
 } 
