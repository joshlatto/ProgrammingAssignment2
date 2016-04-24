## Defines two functions that allow you to use a special matrix object 
## that can cache it's own inverse


## This function creates the special matrix object that caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
	p_inverse <- NULL
	p_set <- function(y) {
		x <<- y
		p_inverse <<- NULL 
	}
	p_get <- function() x
	p_setInverse <- function(inverse) p_inverse <<- inverse
	p_getInverse <- function() p_inverse
	list( set = p_set, get = p_get,
	setInverse = p_setInverse,
	getInverse = p_getInverse)
}


## This function computes the inverse of one of our special]
## matrix objects and caches it.
## If the inverse is already cached, it retreives it 

cacheSolve <- function(x, ...) {
	p_inverse <- x$getInverse()
	if(!is.null(p_inverse)) {
		message("Getting cached data")
		return(p_inverse)
	}
	data <- x$get()
	p_inverse <- solve(data, ...)
	x$setInverse(p_inverse)
}

