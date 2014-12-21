##The pair of functions given below are meant to cache the inverse of a given matrix.
##Caching a process such as matrix inversion, normally an expensive routine, helps
##bring down the computational costs.

##When the function makeCacheMatrix is called on an R matrix object, it creates an R
##(list) object containing the value of the matrix and the methods/functions set,get,
##setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(nx){
        x <<- nx
        i <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setinverse <- function(ni){
        i <<- ni
    }
    
    getinverse <- function(){
        i
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##The function cacheSolve is used to calculate the inverse of a given matrix, and check
##if its value happens to be cached or not. The object passed to cacheSolve is the one
##obtained from execution of the previous function, makeCacheMatrix. If it happens to
##be the first call on that particular object, as checked by makeCacheMatrix, the inverse
##of the matrix is calculated, and subsequently stored. Otherwise, if the function has 
##been called before, the cached value is returned.

cacheSolve <- function(x, ...) {
    i <- x$getinverse
    
    if(!is.null(i)){
        message("getting cached data")
        return (i)
    }
    
    input <- x$get()
    i <- solve(input)
    x$setinverse(i)
    
    return (i)
}