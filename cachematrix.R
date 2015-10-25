## Matrix inversions take a lot of time, usually if we have to run it a loop which would imply
## it being computed repeatedly. The functions below try to reduce that computation time by 
## caching(saving) the inverse of the matrix and calling the result directly if asked again in a loop
## thus not requiring us to compute it again.

## This function creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) 
  {        i <- NULL
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

## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x) 
  { i <- x$getinverse()
if(!is.null(i)) {
  message("Getting cached data")
  return(i)
}
data <- x$get()
i <- solve(data)
x$setinverse(i)
i
}
## Return a matrix that is the inverse of 'x'
