
## There are 2 functions here: MakeCacheMatrix and cacheSolve. 
## MakeCacheMatrix function will create a list of global values for an
## input matrix and the cacheSolve function will either return the  
## cached value of the inverse matrix or will calculate and return 
## the inverse of the input matrix.

## The makeCacheMatrix function will create a list of global variables
## related to the inverse matrix value so that the cacheSolve function can
## access them. These values include the input matrix, an empty variable to 
## hold the inverse matrix value, an empy variable to pull the cached inverse
## matrix value.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <- NULL
    
  }
  
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## The cacheSolve function will determine if the inverse of the input matrix 
## (argument x) has been solved already. If it has been solved for, the 
## the function will retrieve the cached value and will return it along with a
## message indicating the value has been cached. If the inverse has not been 
## solved for, the function will calculate and return the inverse value.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  
  if(!is.null(i)) {
    message("getting cached matrix inverse")
    return(i)
    
  }
  
  inputMatrix <- x$get()
  i <- solve(inputMatrix)
  x$setinv(i)
  i
}
