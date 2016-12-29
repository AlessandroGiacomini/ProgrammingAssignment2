# makeCacheMatrix. This function creates a special
# "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
	
	m <- NULL
	
	# set the value of a matrix
	setMat <- function(val) {
		x <<- val
		m <<- NULL
	}
	
	# get the value of a matrix
	getMat <- function() x
	
	# cache the given value (inverse of the matrix)
	cacheInv <- function(solve) m <<- solve
	
	# get the cahced value (inverse of the matrix)
	getInv <- function() m
	
	# return a list
	list(setMat = setMat,
	getMat = getMat,
	cacheInv = cacheInv,
	getInv = getInv)
}

# The following function calculates the inverse of a "special"
# matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
	
	# get the cached value, if it exists return it
	m <- x$getInv()
	
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	# otherwise it get the matrix,
	# calculate the inverse via the "solve" function
	# and store it in the cache
	data <- x$getMat()
	m <- solve(data)
	x$cacheInv(m)
	
	# return the inverse matrix
	m
}