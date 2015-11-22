## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	p_inverse <- x$getInverse()
	if(!is.null(p_inverse)) {
		message("Getting cached data")
		return(inverse)
	}
	data <- x$get()
	p_inverse <- solve(data, ...)
	x$setInverse(p_inverse)
}

