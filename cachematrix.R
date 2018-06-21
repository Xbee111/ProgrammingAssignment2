#Functions to cache the inverse of a matrix

#Creates matrix to cache an inverse
makeCacheMatrix <- function( m =  matrix()){
  i <- NULL
  
  #set the matrix
  setMeth <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  
  #get the matrix
  getMeth <- function(){
    m
  }
  
  #set the inverse matrix
  setInve <- function(inv){
    i <<- inv
  }
  
  #get the inverse matrix
  getInverseM <- function(inverse){
    i <<- inverse
  }
  
  #return list of methods
  list (setMeth=setMeth, getMeth=getMeth, setInve = setInve, getInverseM = getInverseM)
}

#Perfoms a computation of the inverse matrix, if the inverse is computated, 
#then this method will retrieve the inverse matrix from the cache
cacheSolve <- function(x, ...){
  
  # return the inverse of x
  m <- x$getInverseM()
  
  #return matrix already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  #get the matrix
  data <- x$getMeth()
  
  #calculate inverse
  m <- solve(data) %*% data
  
  #set inverse
  x$setInv(m)
  
  #return the matrix
  m
}