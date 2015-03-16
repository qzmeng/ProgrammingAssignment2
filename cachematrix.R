## cachematrix.R - Matrix inversion with memoization function
##
## USAGE
## > 
## > source("cachematrix.R")
## > 
## > c=rbind(c(1, -1/4), c(-1/4, 1))
## > ca=makeCacheMatrix(c)
## > class(c)
## [1] "matrix"
## > class(ca)
## [1] "list"
## > 
## > cacheSolve(ca)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(ca)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 

## Turn a matrix into a matrix caching list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
