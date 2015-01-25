## Put comments here that give an overall description of what your
## functions do
# The functions will calculate the inverse of a matrix, If the matrix 
# has been cached before, the inverse of a matrix will be returned
# instead than compute it again.
## Write a short comment describing this function
# This function 'makeCacheMatrix' creates a special matrix, which is
# a list containing a function to do several things
# * Sets the value of the matrix
# * Gets the value of the matrix
# * Sets the inverse of the matrix
# * Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y)
{
x <<- y
m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## Write a short comment describing this function
# This function 'cacheSolve' computes the inverse of the special matrix
# created with the function above. First it checks in the inverse of the
# matrix has been already calculated. If this is the case, it returns the
# the inverse matrix from the cache and skips the computation. Otherwise,
# it calculates the inverse of the matrix and sets the inverse matrix in
# the cache by means of the 'setinverse' funcion.
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
