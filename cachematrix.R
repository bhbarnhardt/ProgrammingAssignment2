## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##COMMENT: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # An empty vetor m is created in the cache
        get <- function() x # This creates the function get(), which calls matrix x.
        setinverse <- function(solve) m <<- solve # This function saves the inverse of matrix x to the cache.
        getinverse<- function() m # This calls the inverse of matrix x from the cache.
        list(get = get,               # This compiles functions get, setinverse and getinverse onto a single list.
             setinverse = setinverse, # Each argument corresponds to the result of the function of the same name.
             getinverse = getinverse)
}

## Write a short comment describing this function

## COMMENT: This function takes as input the 'special vector' f, which is actually a list of functions related to matrix x,
## created by makeCacheMatrix(). This function either returns the inverse of the matrix x, or calculates an inverse
## for matrix x which it returns. 

cacheSolve <- function(f, ...) {
        m <- f$getinverse()  # This checks the value of vector m in the cach
        if(!is.null(m)) {    # This states that if m is NOT null, the next lines of code should be executed.
                message("Getting cached data...") # The message "getting cached data" is returned.
                return(m) # m is returned from the cache.
        } else {
                message("Not in cache. Caluclating matrix inverse...")
        }
        data <- (f$get()) # If the inverse is not already stored as vector m in the cache, matrix x is stored in vector 'data'.
        m <- solve(data, ...) #solve() inverts matrix x, which has been stored in vector 'data'. The result is stored in the cache.
        f$setinverse(m) # The inverse is saved to the cache as m.
        m # m is returned.
}
