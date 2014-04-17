## These functions are used to save time when computing the inverse
## of a matrix when this computation is likely to be repeated. This
## is done by caching the value of the inverse so it does not have to
## be recomputed each time the inverse is needed.

## The makeCacheMatrix function creates a special "matrix" that is
## actually a list containing functions that do the following:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The cacheSolve function returns the inverse of the special "matrix"
## created by the makeCacheMatrix function above. It first check to see
## if the inverse has already been calculated. If so, it returns the cached
## value. If not, it calculates the inverse, and caches the calculated
## value.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv 
}
