## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# I use the variable x as the input of our matrix
# Then I do the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value if the inverse matrix
# 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# This function is getting the cache data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {   # Check if the inverse has already been calculated
      message("getting cache data!")
      return(inv)       
  }
  data <- x$get()
  inv <- solve(data, ...) #Calculate the inverse value
  x$setinverse(inv)
  inv
}
