
## Write a short comment describing this function

## The makeCacheMatrix creates a special matrix which
  ## 1. Set the value of the matrix
  ## 2. Get the value of the matrix
  ## 3. Set the value of the inverse of the matrix
  ## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    }


## The CacheSolve function verifies if the inverse of the matrix was previously calculated.
## If yes, it gets the inverse from the cache. If not, it calculates the inverse of the data
## and sets the value of the inverse in the cache through the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return (i)
        
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    i
}
