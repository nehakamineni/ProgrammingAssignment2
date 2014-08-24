
## makeCacheMatrix function creates a matrix object 

makeCacheMatrix <- function(x = matrix()) {
  inv_xmat = NULL
  
  #set matrix function
  set <- function(y) {
    x <<- y
    inv_xmat <<- NULL       ##initializing inverse matrix to null
  }
  
  #Get Matrix function
  get <- function() x
  
  #Set inverse of matrix function
  setinverse<- function(inverse) inv_xmat <<-inverse
  
  #Get Inverse of matrix function
  getinverse <- function() inv_xmat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns inverse of matrix
## If inverse is avaialbe in cache, cacheSolve function retreives else it
## computes it and sets the cache

cacheSolve <- function(x) {
  
  ## Returns the inverse of matrix x 
  xinv = x$getinverse()
  
  #Checks if cache has any value
  if (!is.null(xinv)) {
    message("Displaying cached inverse of the matrix")
    return(xinv)
  } 
  else {
    xinv = solve(x$get())
    x$setinverse(xinv)
    return(xinv)
  }
}

# t=makeCacheMatrix(matrix(rnorm(10000),100,100))

# cacheSolve(t)

# p=solve(matrix(rnorm(10000),100,100))

