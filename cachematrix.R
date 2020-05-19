## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly

## Here two functions were constructed to inverse and cache a matrix

## makeCacheMatrix() creates a special vector (list of four function) to get, set, 
## getinverse, and setinverse of a matrix

makeCacheMatrix <- function(x=matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() will inverse the matrix from makeCacheMatrix() using solve() function.
## It will also cache the output in the environment

cacheSolve <- function(x,...){
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}