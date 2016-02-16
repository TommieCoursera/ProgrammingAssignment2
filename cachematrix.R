# ------------------------------------------------------------------------
#   R Programming - Assignment #2
#   (1) makeCacheMatrix: a function that creates a list of sub-functions: set, get, setinv and getinv
#   (2) input of makeCacheMatrix is a matrix
#   (3) cacheSolve: a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above
#   (4) please see the last section of this program for a test run
# ------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv= setinv, getinv = getinv)
}

cacheSolve <- function(x, ...){
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
}

# this is how we test the program:
m <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = FALSE) # declare a matrix
m_test_inv <- makeMatrix(m)
cacheSolve(m_test_inv) # output  

# this is how we compare above result with R's generic function "solve":
X <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = FALSE)
solve(X) # output
