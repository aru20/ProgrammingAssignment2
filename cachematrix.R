##11/28/2020
## Matrix inversion usually take some time. In below function we use caching technique.
## If the contents of the matrix is not changing, it will show the cached value instead of computing it again.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
       x <<- y
       inv <<- NULL
    
    }
    get <- function() x
    setinverse <- function(inverse) inv <<-inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse =  setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated
##and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinverse()
    if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
    }
    mat.data <- x$get()
    inv <- solve(mat.data, ...)
    x$setinverse(inv)
    return(inv)
  }
  inv1 <- makeCacheMatrix(matrix(c(1:4),nrow=2,ncol=2))
  cacheSolve(inv1)
  

