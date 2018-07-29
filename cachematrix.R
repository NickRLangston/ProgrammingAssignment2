## The following 2 functions can be used to compute the inverse of an
## invertible matrix. The inverse is cached and if cacheSolve is used
## to return it again, the inverse will be returned from the cache 
## without computing it again.


## Given a matrix, this function returns a list of functions allowing the
## user to set or get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}




## Given a list returned from the makeCacheMatrix function, this function
## returns the inverse of the matrix that was given to makeCacheMatrix,
## returning the inverse from the cache instead of computing it when possible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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

## Week 3 Programming Assignment, JHU R Programming on Coursera
## 7:37pm, 7-28-18, Saturday
## Nick