## In conjunction, both these functions help find the inverse
## of a matrix, given that finding the inverse of a matrix
## is a resource consuming task, when the inverse has
## already been calculated the data is retrieved from cache.
##
## SAMPLE USAGE:
## mat1 <- matrix(runif(25),5,5)
## mat <- makeCacheMatrix(mat1)
## cacheSolve(mat)
## mat1 %*% cacheSolve(mat)


## This function creates a matrix object with the ability to
## store it's own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inversem <- NULL
    set <- function(y) {
      x <<- y
      inversem <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inversem <<- solve
    getinverse <- function() inversem
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of the matrix created
## with the above function and stores it in cache,
## if it's been calculated before, it retrieves
## the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversem <- x$getinverse()
  if(!is.null(inversem)) {
    message("getting cached data")
    return(inversem)
  }
  data <- x$get()
  inversem <- solve(data, ...)
  x$setinverse(inversem)
  inversem
}

