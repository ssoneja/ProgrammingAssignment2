## makeCacheMatrix: This function takes a matrix and creates its inverse 
## if the inverse not present in the cache 
## otherwise returns the inverse present in  the cache
## cacheSolve: This function takes a matrix and checks if the inverse is
## present in the cache. If the inverse is present, then it returns the values from 
## the cache. If the inverse if not present then, it calculates the inverse using
## the solve function, stores the inverse in the cache, and returns the inverse

## makeCacheMatrix: This function takes a matrix and creates its inverse 
## if the inverse not present in the cache 
## otherwise returns the inverse present in  the cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- x
        inv <- NULL
        
        get <- function() {x}

        setinv <- function (inv) {inv <<- solve(x)}
        getinv <- function () {inv}
                
        list(get=get,
               setinv = setinv,
               getinv = getinv)
}


## cacheSolve: This function takes a matrix and checks if the inverse is
## present in the cache. If the inverse is present, then it returns the values from 
## the cache. If the inverse if not present then, it calculates the inverse using
## the solve function, stores the inverse in the cache, and returns the inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m = matrix()
        m <- x$getinv()
        if(!is.null(m)){
                message ("Getting cached data")
                return (m)
        }
        
        dat <- x$get()
        m <- solve(dat, ...)
        x$setinv(m)
        return (m)
        
}
