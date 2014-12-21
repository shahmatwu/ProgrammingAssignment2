## Create a special matrix-based object that caches its inverse

## Create a special "matrix", which is really a list of functions
## for caching (storing) and retrieving a matrix and its inverse - namely
## 1. set - store a matrix
## 2. get - retrieve the stored matrix
## 3. setinverse - store the inverse of the matrix - NOTE: assumes matrix is invertible
## 4. getinverse - retrieve the stored inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #matrix inverse cache
    
    # define the accessor functions
    set <- function(y){
        x <<- y # set new value of x in parent i.e. in the environment that defined this list$set function
        inv <<- NULL # and clear the cached inverse
    }
    get <- function() {x}
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() {inv}
    
    # now return the list of accessor functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returning the cached inverse of the special matrix object or (if none in the cache)
## calculate, store in cache and return the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
