## This function creates a special "matrix" object 
## which can cache its inverse
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(
                set=set, 
                get=get, 
                setinverse=setinverse, 
                getinverse=getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## It first checks whether the inverse has been calculated or not
## If yes, gets the result and skips the computation
## If not, it calculates the inverse of the matrix and 
## sets the value in the cache via setinverse function

cacheSolve <- function(x, ...) {
        ## assume the matrix is invertible
        inv <- x$getinverse()  
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
