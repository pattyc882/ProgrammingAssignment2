## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Creates makeCacheMatrix function, initializes variables, creates set, get, 
#set inverse and get inverse functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#If the inv variable is not null, inv is pulled from cached data.
#If the inv varialbe is null, it uses the get function created in makeCacheMatrix
#to retrieve the value of x from cache and assign it to the variable data.  Solve
#is then used to create inverse of data and value is assigned to inv.
# it then uses the setinverse function from makeCacheMatrix to cache the inverted matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
