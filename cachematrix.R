# The function makeCacheMatrix() gets a matrix as an argument and it returns
# a list of functions set(),get(),setinv(),getinv() that set/get the matrix 
# and its inversed matrix respectively
makeCacheMatrix <- function(x = matrix()) {
        i <- matrix()
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The function cacheSolve() calculates the inverse matrix of the list
# that is returned by previous function. It first checks to see if the
# inversed matrix exits already in cache. If yes, ten it returns the matrix
# from cache, otherwise it calculates the inversed matrix and caches it
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.na(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
