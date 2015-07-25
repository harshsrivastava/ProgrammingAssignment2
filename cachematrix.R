## Put comments here that give an overall description of what your

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve - will return the inverse of the passed matrix. It will first try to find 
## if an inverse matrix exists in cache or not. If not, it will calculate the inverse of the passed 
## matrix and put it to cache

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    check <- x$get ##check if the same matrix was passed
    if(check == x) ##if the matrix is the same as the one for calculating the cached inversem retrive from cache
    {
        message("retriving from cache")
        i <- x$getinverse()
        return(i)
    }
    ##if inverse was not found from the cache, proceed to calculate and set in cache
    data <- x$get()
    #Calculating the inverse
    i <- solve(data)
    x$setinverse(i)
    ##return the inverse and print
    i
    
}
