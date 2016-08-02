## Put comments here that give an overall description of what your
## functions do

## This function returns the following list of functions:
## set -> Set the  Value of a Matrix
## get -> Get the value of a Matrix
## setsolve -> Set the value of the inverse
## getsolve -> Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setsolve <- function(solve) inv <- solve
  getsolve <- function() inv
  
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
  

}


## This function calculates the inverse of the matrix calling the functions exposed by makeCacheMatrix cacheSolve(makeCacheMatrix(A))
## It checks if the inverse has been already calculated if (!is.null(inv)). 
## And if not it will calculate the inverse and will save it in the "cache" memory x$setsolve(inv)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if (!is.null(inv)) {
    message("getting  cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setsolve(inv)
  inv
}
