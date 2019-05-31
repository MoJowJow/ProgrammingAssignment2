## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(Matrix)

makeCacheMatrix <- function(x = matrix()) {
  Inv_Matrix <- NULL
  
  Set_Matrix <- function(matrix){
    x <<- matrix
    Inv_Matrix <<- NULL
  } #Storing the passed matrix and checks whether the matrix is already cached
  
  Get_Matrix <- function() x
  
  
  Inversing_Matrix <- function(imatrix){
    Inv_Matrix <<- imatrix
  } #setting the value of Inv_Matrix in the parent environmet
  
  Get_iMatrix <- function(){
    Inv_Matrix
  }
  
  list(Set_Matrix= Set_Matrix, Get_Matrix = Get_Matrix, Inversing_Matrix = Inversing_Matrix, Get_iMatrix = Get_iMatrix) #for accessibility of the functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  Inverse <- x$Get_iMatrix()
  
  if(!is.null(Inverse)){
    return(Inverse)
  } #returns an inversed matrix if it is already cached 
  
  else {  
    Output <- x$Get_Matrix()
    Inverse <- solve(Output)
    x$Inversing_Matrix(Inverse)
    return(Inverse)
  } #computes for the inverse of the matrix given it is not yet cached
  
}

