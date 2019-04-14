## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function takes a matrix x and returns
##a list of functions, which serve to cache the
##data of x.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

##This function takes in a list given by makeCacheMatrix.
##If the inverse of the matrix given in the list has
##already been solved for, it will return the cached data.
##Otherwise it will compute the inverse of the matrix and
##cache the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
          message("getting cached data")
          return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
