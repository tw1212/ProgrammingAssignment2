## These functions will enable the cache and reuse of the potentially time 
## consuming operation of inversing a matrix.  
## These functions assume that the matrix passed is always invertible

## This function creates a matrix object with four methods
## set and get ill set and return the matrix
## setInverse will persist the inverse in cache
## getInverse will return the inverse that was saved in cache 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function will first test if the inverse has already been calculate and 
## saved to cache.  If it is in cache it will return that value if not it will 
## compute the inverse of the matrix and call the setInverse method of the 
## special matrix object to cache that value for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
