## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.
## The function cacheSolve  function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.


## This function retrieves a cached matrix if it exists
## If this is the 1st time the function is called it will create the matrix
## This function is really a list containing a function to
## 1.  set the value of the square matrix
## 2.  get the value of the square matrix
## 3.  set the value of the inverse of the square matrix
## 4.  get the value of the inverse of the square matrix
makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix <- NULL
    set <- function(y) {
        x <<- y
        cachedMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(setInverse) cachedMatrix <<- solve
    getInverse <- function() cachedMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function will calculate and return the inverse of a square matrix created
## with the function makeCacheMatrix
## If the inverse of the matrix has previously been calculated, it will return the 
## same value and skip the solve computation.
## This function assumes the matrix provided is always inversible and therefore does
## not attempt any special error handling for a matrix that is not inversible
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Use solve to return inverse, always assuming matrix is inversible    
    solvedCache <- x$getInverse()
    if (!is.null(solvedCache)) {
        message("getting cached data")
        return(solvedCache)
    }
    solvedCache <- x$get()
    solvedCache <- solve(solvedCache)
    x$setInverse(solvedCache)
    solvedCache
}
