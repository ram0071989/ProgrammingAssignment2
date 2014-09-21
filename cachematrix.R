## Put comments here that give an overall description of what your
## functions do

# This function takes an input argument
# as a matrix and then computes the Inverse
# of the matrix and stores it in the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(b) {
    x <<- b
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInv <- function(calcInv) {
    inv <<- calcInv
  }
  getInv <- function() {
    inv
  }
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


# This function takes an input argument as a matrix
# created using the makeCacheMatrix and outputs the
# Inverse of that matrix if it is in cache else it
# computes the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invRs <- x$getInv()
  if(!is.null(invRs)) {
    message("Retrieving cached matrix inverse")
    return(invRs)
  }
  ip <- x$get()
  invRs <- solve(ip,...)
  x$setInv(invRs)
  invRs
}
