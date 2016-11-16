# R Programming Week 3 Assignment #

# Creating Inverse Matrix #
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                               ##set null value to initialize matrix
        set <- function(y) {                                    ##set matrix x with function
                x <<- y
                m <<- NULL
        }
        get <- function() x                                     ##return matrix x
        setinverse <- function(solve) m <<- solve               ##set inverse of matrix x
        getinverse <- function() m                              ##get inverse of matrix x
        list(set = set, get = get,                              ##define functions
                setinverse = setinverse,
                getinverse = getinverse)
}
                
# Compute the Inverse of the above #
cacheSolve <- function(x, ...) {
        m <- x$getinverse()  
        if(!is.null(m)) {                                       ##check for stored matrix inverse
          message("getting cached data")    
          return(m)                                             ##return the matrix inverse
        }  
        data <- x$get()
        m <- solve(data, ...)                                   ##output the matrix inverse
        x$setinverse(m)  
        m
}
