
#makeCacheMatrix: This function creates a special "matrix" object that can cache it's inverse .
#The main purpose is to store a matrix and its inverse, allowing for retrieval of and computation of the inverse only when necessary (this saves computational time .)


makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse variable to NULL
  inv<-NULL
  #function for setting the matrix
  #this updates the matrix and resets the cached inverse 
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  #Function to get the matrix
  #returns the stored matrix 
  get<-function()x
  #function to set the inverse of the matrix 
  #this is to store the computed inverse in the cache 
  setInverse<-function(inverse)inv<<-inverse
  #function to get the inverse returns the cached inverse if available
  getInverse<-function()inv
  
  #return a list of the above functions to manage the matrix and its inverse 
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}


#cacheSolve:this function computes the inverse of a special 'matrix"created by MakeCacheMatrix.
#if the inverse is already cached , it retrieves the cached value to avoid redundant computation.
#otherwise it computes the inverse , stores it in the cache and returns it .

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    #notify the user that the cached data is being used 
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  #return a matrix that if the inverse of 'x'
  inv
}