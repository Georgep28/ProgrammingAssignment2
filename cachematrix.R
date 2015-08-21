#Running Example
#a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#a$GetMatrix()
#a$GetInverseMatrix()
#cacheSolve(a)


## Write a short comment describing this function
#makeCacheMatrix is a function that returns a list that contains the following functions
#SetMatrix: Set the values of the matrix
#GetMatrix: Get the values of the matrix
#SetInverseMatrix: Set the Inverse values of a matrix
#GetInverseMatrix: Get the Inverse Values of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #Matrix Is set to NULL
  m <- NULL
  
  #SetMatrix will set the matrix Set 
  SetMatrix <- function(y)
  {
    x <<- y
    m <<- NULL
    
  }
  #Simply return the Matrix
  GetMatrix <- function()
  {
    x
  }
  
  #Set Inverse Matrix
  SetInverseMatrix <- function(InverseMatrix)
  {
    m <<- InverseMatrix
  }
  
  #Simply Return Inverse Matrix
  GetInverseMatrix <- function()
  {
    m
  }
  
  #Return list of Objects
  list( GetMatrix = GetMatrix, SetInverseMatrix = SetInverseMatrix, GetInverseMatrix = GetInverseMatrix)  
  
}


## Write a short comment describing this function
#cacheSolve is a functin that either calculates the inverse of a matrix or returns the cached one
#If the inverse has been calculated before, it returns the cached one else it calcualtes the inverse
#save it in the cache and then returns it

cacheSolve <- function(x, ...) {
  
  ## Get the matrix and set it to m
  m <- x$GetInverseMatrix()
  
  #check if the inverse has been calculated before and it is in the cache. If yes simply return it.
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  #Calculate the inverse
  data <- x$GetMatrix()
  m <- solve(data)  
  x$SetInverseMatrix(m)    
  
  ## Return a matrix that is the inverse of 'x'
  m  
    
}
