## Author: tim finholm
## makeCacheMatrix builds the cache matrix from user-supplied input and sets the inverse to null, to avoid using a cached value later
## cacheSolve gets the inverse of the matrix and returns it

## this function builds the matrix (take the red pill)
makeCacheMatrix <- function(x = matrix()) {
    if(!is.matrix(x)){     # test that x is a matrix
    	message("x is not a matrix, exiting")
    	stop
    }
    inverse <- NULL         # start with a null inverse
    matrix <- function(y) { 
        x <<- y             # let x take y
        inverse <<- NULL    # and re-set the inverse to null
    }
    get <- function() x     # get x
    setinverse <- function(flip) inverse <<- flip  # set the inverse
    getinverse <- function() inverse               # get the inverse
    list(matrix=matrix, get=get, setinverse=setinverse, getinverse=getinverse) # return a list
}

# solve the cache if necessary
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()  # get the inverse
    if(!is.null(inverse)) {    # don't solve anything if the cached data is still valid (inverse would be set to null if the matrix is new) 
        message("using cached data.") # let the user know cached data is being used
        return(inverse)        # return the inverse
    }
    else {                     # if the inverse is null, we have work to do
    	values <- x$get()         # get the values
    	inverse <- solve(values)  # solve the matrix
    	x$setinverse(inverse)     # set the inverse for later use if possible
    	return(inverse)           # return the inverse to the calling function
    	}
}