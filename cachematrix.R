## Put comments here that give an overall description of what your
## functions do

##COMMENT: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # An empty vetor m is created in the cache
        set <- function(y) { #This anonymous function assigns y to vector x and NULL to m.
                x <<- y
                m <<- NULL
        }
        get <- function() x # This stores x as vector get.
        setinverse <- function(solve) m <<- solve #This stores the inverse of the matrix in the cache as vector m.
        getinverse<- function() m #This returns m.
        list(set = set, get = get, #This lists the functions set, get, setinverse and getinverse, and identifies their environment.
             setinverse = setinverse,
             getinverse = getinverse)
}


## COMMENT: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()  # This checks the value of vector m in the cache
        if(!is.null(m)) {    #If m is NOT null, the next lines of code are executed.
                message("getting cached data") # The message "getting cached data" is returned.
                return(m) # m is returned from the cache.
        }
        data <- x$get() #If the inverse is not already stored as vector m in the cache, it is computed from the matrix.
        m <- solve(data, ...) #solve() inverts the matrix x, which has been saved to vector data. The results is stored in the cache as m.
        x$setinverse(m) # The inverse is saved to the cache as m.
        m # m is returned.
}
