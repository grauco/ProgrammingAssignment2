## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The two functions below create a special object that create a matrix and
## cache its inverse.

## This function creates a special "matrix" object that can cache its inverse: 
## creates a list containing a function to 
## (i) set the value of the vector and of the mean 
## (ii) get the value of the vector and of the mean

makeCacheMatrix <- function(x = matrix()) {

 	m <- NULL
  	set <- function(y) {
    	    x <<- y    
    	    m <<- NULL 
  	}
 
	get <- function() x
  	
  	setinverse <- function(inverse) m <<- inverse
  	getinverse <- function() m
  
	list(set = set, get = get,
       	setinverse = setinverse,
       	getinverse = getinverse)
  
}


## This second function calculates the inverse of the list created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
	m <- x$getinverse()
  
	if(!is.null(m)) {
    		message("getting cached data")
    		return(m)
  	}
  
	data <- x$get()
  	m <- solve(data, ...)
  	x$setinverse(m)
  
	m

}
