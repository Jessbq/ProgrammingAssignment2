
#Programming assignment using lexical scoping
#R code that is able to cache potentially time-consuming computations
#Function pair that can cache the inverse of a matrix

##makeCacheMatrix creates a special matrix that can cache its universe

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #set the matrix
        setmatrix <- function (y){
                x <<- y
                m <<- NULL
        }
        
        #get the matrix
        getmatrix <- function() x
        
        #set the inverse of the matrix
        setinverse <- function(solve) m <<- solve
        #get the inverse of the matrix
        getinverse <- function() m
        
        #create list
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


##cachesolve computes the inverse of the special matrix returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$getinverse()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

