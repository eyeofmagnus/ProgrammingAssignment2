## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse
  inv <- NULL           ##inv as null
  set <- function(y) {  ##define set function to assign new 
    x <<- y             ##value of matrix in parent enviro
    inv <<- NULL        ##new matrix reset inv to NULL
  }
  get <- function() x   ##defein 'get' to return matrix agrument value
  setinverse <- function(inverse) inv <<- inverse ## assign value if inv in par envo
  getinverse <- function() inv
                        ##define function to get the inverse
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  ##to call functions with"$"
}

## Write a short comment describing this function
## this function computes the inverse of the special "matrix" returned by above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
