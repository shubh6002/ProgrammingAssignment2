

## makeCacheMatrix function creates special matrix ie. x
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <- function(y){
    x<<- y
    inv<<- NULL
}
get <- function()x
setInverse <- function(solveMatrix) inv <<- solveMatrix
getInverse <- function() inv
    list(set = set, get = get,
    setInverse = setInverse, 
    getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cache solve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv   
}
