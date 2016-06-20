## Hello! 
##The function makeCacheMatrix() creates a matrix that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                
                x <<- y
                
                inv <<- NULL
                
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
        
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## The function cacheSolve() takes the output of makeCacheMatrix and 
## give its inverse

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                
                message("getting cached data") #Because the inverse has been already been calculated
                
                return(inv)
                
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setInverse(inv)
        
        inv
        
}

