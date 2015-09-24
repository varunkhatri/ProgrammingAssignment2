makeCacheMatrix=function(x = matrix()){
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

cacheSolve=function(x=matrix(),...){
        
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