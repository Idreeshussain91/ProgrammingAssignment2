## These functions efficiently find the inverse of a matrix if one exists.

## Returns a List of functions that set and get values from an environment.




makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #    1. Set matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #    2. Get matrix
        get <- function() x
        
        #    3. Set matrix inverse
        setinverse <- function(inverse) m <<- inverse
        
        #    4. Get matrix inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Finds the inverse of a matrix. 
## If the inverse already exists in the cache then retrieves that.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        #Check if inverse exists in Cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #else compute inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        # Return a matrix that is the inverse of 'x'
        m
        
}
