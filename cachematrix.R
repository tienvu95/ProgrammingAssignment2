## Put comments here that give an overall description of what your
## functions do
## Aim is to create a pair of functions that cache the inverse of matrix


## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse (copied from the instruction)
makeCacheMatrix <- function(x = matrix()) {
  invt <- NULL
  #get/set matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #get/set invert matrix
  setInvt <- function(inverse) invt <<- inverse
  getInvt <- function() invt
  list(set = set, get = get, setInvt = setInvt, getInvt = getInvt)
}

## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache (copied from the instructions)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## x here will be the output of makecachematrix function created above
  invt <- x$getInvt()
  if(!is.null(invt)){
    message("getting cached data")
    return(invt)
  }
  matrixdata <- x$get()
  invt <- solve(matrixdata)
  x$setInvt(invt)
  
  return(invt)      
}


