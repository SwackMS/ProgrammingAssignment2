##cachematrix object, 4 functions, 2 properties
##set, get for matrix
##set, get for inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  #initialize inverse placeholder
  inverse <- NULL
  #set matrix and initialize inverse value
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #return matrix
  get <- function() x
  #set inverse
  setInverse <- function(i)
    inverse <<- i
  #return inverse
  getInverse <- function()
    inverse
  
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##matrix parameter must be invertible
##solves matrix to inverse calculation
##requires matrix as parameter
##skips calculations if inverse already generated
##sets cache if calculated
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  #if there is not a null inverse
  if (!is.null(inverse)) {
    message("getting cached data")
    #then return the cached inverse
    return (inverse)
  }
  #else calculate
  #gather data
  data <- x$get()
  #solve
  inverse <- solve(data)
  #set
  x$setInverse(inverse)
  #return inverse
  inverse
}
