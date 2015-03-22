## This function is to return a matrix that is the inverse of matrix 'x'
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## (assume that the matrix supplied is always invertible)

## makeCacheMatrix function create a special vetor 
## which is a list containing functions and get and set matrix and inverse

makeCacheMatrix <- function(x = matricx()) {
  
  #inital the inverse varible to null
  inverse <- NULL  
  
  #set the matricx
  set <- function(y){
    
    #Set 'x' for the function enviromnent to 'y'    
    x <<- y
    #Set 'm' for the 'makeCacheMatrix' parent environment to NULL    
    inverse <<- NULL
    
  }
  
  # get the matricx
  
  get <- function() x
  
  ##Takes a value solve and sets it to the
  #value inverse in the 'makeCacheMatrix' environment
  setinverse <- function(solve = NULL) {
    inverse <<- solve
  }
  # return the value of inverse in the 'makeCacheMatrix' environment
  getinverse <- function() inverse
  
  ##Lists out the values of the functions in the 'makeCacheMatrix' environment
  #so that they can be accessed using $
  
  list( get = get, set = set, setinverse = setinverse, getinverse = getinverse)
  
}



## cachSolve calculates the inverse of the special matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  # Goes to the'x' environment and assigns the 
  # inverse value from that environment to inverse  
  inverse <- x$getinverse()  
  
  ## message to check cache
  message("Checking cache...") 
  
  ##If the 'x' environment has been evaluated 
  # before, the function prints the message and the value of inverse (the cached inverse),
  # othrewise prints the message about no previouse cached value
  # and caculate and set the new cache value
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  else {
    message("No previously cached value.")  
    
    # measage set new cached value
    message("Setting new cached value...") 
    
    #assign x matrix to local varible data
    data <- x$get()
    
    # Calculate the inverse of the matrix x by calling
    # 'solve' function on the data local variable    
    inverse <- solve(data, ...)
    
    # Assign the calculated inverse to the 'x'
    # environment using the 'setinverse' function
    x$setinverse(inverse)
  }
  
  #display the inverse
  inverse
  
}
