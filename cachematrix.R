## Code for caching the matrix inverse 


## Caching logic is implemented in this function

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        setInverse<- function(inv){
                i <<- inv 
        }
        
        getInverse<- function() {
                i
        }
        
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## function that returns matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		i<- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        message("No cache set")
        matrix<-x$get()
        i<-solve(matrix, ...)
        x$setInverse(i)
        
        i
}
