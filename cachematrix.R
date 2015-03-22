## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makes 4 functions: set, get, setinverse, getinverse
##   set moves the input to the parent environment as x
##   get retrieves x from the parent environment
##   setinverse moves the solved input to the parent environment as n
##   getinverse retrieves n from the parent enviroment

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) n <<- solve
  getinverse <- function() n
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## 1. gets n from the parent
## 2. checks if n has been solved yet (n is not null)
##      2a. if n is not null, returns the message "getting cached data" and n
## 3. if n is null then calls get on x, solves x as n, calls setinverse on n, prints n


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}
