## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# first you set the value of the matrix
# after that you get the value of the matrix
# from there you set the value of the inverse of the matrix
# and then similarly set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  cache_inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x 
  setinverse <- function(inverse) cache_inv <<- inverse
  getinverse <- function () cache_inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# cachesolve returns the inverse of the matrix thas is provided. 
# it checks if the inverse has already been calculated. If it has been 
# it gets the result and does not compute, otherwise it does compute and
# sets it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  cache_inv <- x$getinverse()
  if(!is.null(cache_inv)){
    message("loading cached data")
    return(cache_inv)
  }
  
  data <- x$get()
  
  result <- try(
    cache_inv <- solve(data)
  )
  
  if(result){
    x$setinverse(cache_inv)
    return(cache_inv)
  }else {
    return("the matrix was not invertible")
  }
}
