## makeCacheMatrix and cacheSolve are two functions that are used to store and cache the inverse of a matrix

## makeCacheMatrix creates a matrix object

makeCacheMatrix <- function(x = matrix()) {
	var = NULL
	set <- function(y) {
	x <<- y
	var <<- NULL
	}
	
	get <- function () x
	SetInverse <- function(inverse)  var <<- inverse
	GetInverse <- function () var
	list (set=set, get=get, SetInverse=SetInverse, GetInverse=GetInverse)
}


##  cacheSolve solves the inverse of the matrix created by the above function. If by chance the inverse has already been calculated,  get the inverse from the cache

cacheSolve <- function(x, ...) {
        var <- x$GetInverse()
		if (!is.null(var)) {
			message("getting cached data")
			return  (var)
		}
		
		data <- x$get()
		var <-solve(data, ...)
		x$SetInverse(var)
		var
}
