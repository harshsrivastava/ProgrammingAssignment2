## Assignment2: Caching the Inverse of a Matrix 
## Function to calc inverse of a matrix. In case the matrix inverse was already calculated, retrieve from cache
## Else calculate inverse and set to cache

## makeCacheMatrix - creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ##
    
    i <- NULL
    set <- function(y) { ##set the value of the matrix to cahce
        x <<- y
        i <<- NULL
    }
    get <- function() x ##get the value of the matrix from the cache
    setinverse <- function(inverse) i <<- inverse ##set the inverse of the matrix to cache
    getinverse <- function() i ##get the inverse of the matrix from cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) ##return object having functions for the four operations
}


## cacheSolve - will return the inverse of the passed matrix. It will first try to find 
## if an inverse matrix exists in cache for the ame matrix or not. If not, 
## it will calculate the inverse of the passed matrix and put it to cache

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
    ##return the inverse, set to cache and print
    i
    
}
