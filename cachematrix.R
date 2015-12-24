# calculate the inverse of matrix using solve function and 
# caching the inverse of matrix 

# makeCacheMatrix function creates an object that can cache input matrix and it's inverse
makeCacheMatrix <- function(mdata = matrix()) {
  
  # cache the input matrix
  setmatrix <- function(set.data) mdata <<- set.data
  
  # return the input matrix from cache
  getmatrix <- function() mdata
  
  mdata.inverse <- NULL
  
  # cache the inverse of input matrix
  setmatrixinverse <- function(set.inversedata) {
    mdata.inverse <<- set.inversedata
  }
  
  # return the inverse of input matrix from cache
  getmatrixinverse <- function() mdata.inverse
  
  list(setmatrix=setmatrix, getmatrix=getmatrix, 
       setmatrixinverse=setmatrixinverse, getmatrixinverse=getmatrixinverse)
}

# cacheSolve function computes the inverse of matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mdata, ...) {
  
  # get matrix inverse from cache
  mdata.inverse <- mdata$getmatrixinverse()
  
  #use solve function to inverse the matrix if it's already not available in cache
  if(is.null(mdata.inverse))  
    mdata.inverse <- mdata$setmatrixinverse(solve(mdata$getmatrix()))
  else
    print("inverse matrix data from cache")

  mdata.inverse
}
