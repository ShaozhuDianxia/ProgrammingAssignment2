## By ShaozhuDianxia: This pair of functions that cache the inverse of a matrix.
## Basically, the first creates a special "matrix" object that can cache its 
## inverse, and the second computes the inverse of the special "matrix" returned
## by the first one.If the inverse has already been calculated (and the matrix
## has not changed),then the cachesolve should retrieve the inverse from the cache.


## ShaozhuDianxia's Description of the makeCacheMatrix Function:
## "makeCacheMatrix" creates a special object, which contains a function to:
## 1.set the value of the matrix;
## 2.get the value of the matrix;
## 3.set the value of the inverse;
## 4.get the value of the mean.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## ShaozhuDianxia's Description of the cacheSolve Function:
## It calculates the inverse of the special object created with the first function.
## However, it first  checks to see if the inverse has already been calculated. If
## so,it gets the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the data and sets the value of the inverse via the
## setmean function.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##ShaozhuDianxia's test case:(Please source the script first.)
##x <- cbind(c(5,3),c(-2,7))
##y <- makeCacheMatrix(x)
##cacheSolve(y)
##Successfully returned the inverse of the matrix x.
