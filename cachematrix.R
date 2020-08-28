## This function calculate the inverse of the matrix, but also 
## cache the calculated inverse matrix so it could be called 
## immediately instead of recalculate it


## initialize the matrix, and also keep the cache inverse matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## calculate the inverse matrix, and also call the stored inverse
## matrix that has been calculated before

cacheSolve <- function(x, ...){
  inv <- x$get_inverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(a = data, ...)
  x$set_inverse(inv)
  inv
}