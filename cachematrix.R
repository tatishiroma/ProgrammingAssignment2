## makeCacheMatrix is a function that creates a special matrix object that can be cache its inverse.

## Here, we use the <<- operator to store the matrix and its inverse in an environment
## that is different from the current environment.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Retrieve the cached matrix inverse. cacheSolve computes the inverse of the special matrix returned by the function above.
## This function will check if inverse is already calculated and if it's not, then it will calculate it and send the results to the cache.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$getinverse(i)
        i
}
