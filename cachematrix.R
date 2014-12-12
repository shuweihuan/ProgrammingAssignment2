## For R Programming Assignment 2

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() {
		x
	}
	setInvMatrix <- function(invM) {
		m <<- invM
	}
	getInvMatrix <- function() {
		m
	}
	list(set=set, get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	m <- x$getInvMatrix()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInvMatrix(m)
	m
}
