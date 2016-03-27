#makeCacheMatrix and cacheSolve calculate the inverse of Matrix.
#If the inverse of a Matrix is already calculated the cached results are
#returned instead of recalculating the inverse Matrix.


#makeCacheMatrix creates a special type of list with four functions:
#  setMat: Sets a matrix.
#  getMat: Returns a previously set matrix.
#  setinvMat: Sets the inverse of a matrix.
#  getinVMat: Returns the inverse of a previously calculated inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  setMat <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  getMat <- function() x
  setinvMat <- function(inverse) invMat <<- inverse
  getinvMat <- function() invMat
  list(setMat=setMat, getMat=getMat, setinvMat=setinvMat, getinvMat=getinvMat)
}

#cacheSolve calculates the inverse of a Matrix. If the inverse Matrix has
#already been calculated it retrieves the data from the cache.
cacheSolve <- function(x, ...) {
  invMat <- x$getinvMat()
  if(!is.null(invMat)) {
    message("getting cached data.")
    return(invMat)
  }
  data <- x$getMat()
  invMat <- solve(data)
  x$setinvMat(invMat)
  invMat
}
