## Following functions creates a special type of matrix which caches
## the inverse of the matrix since calculating inverse of a matrix is a 
## costly operation

## Creates a special type of matrix which has its inverse cached into it

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## Getters and setters for the matrix
    set <- function(m) {
        x <<- m
        inverse <- NULL
    }
    get <- function() x
    
    ## Getters and Setters for inverse of the matrix
    setInverse <- function (i) inverse <<- i
    getInverse <- function() inverse
    
    ## Returns a list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## Returns the inverse of the matrix x . If there is a cached copy of matrix returns
## it instead of recomputing it

cacheSolve <- function(x, ...) {
        
    inv <- x$getInverse() 
    if (! is.null(inv) ){
      message("Returning cached inverse of the matrix")
      return(inv)
    }
    
    matrix <- x$get()
    ## Use inbuilt solve method to compute the inverse
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
}
