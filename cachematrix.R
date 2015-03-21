## This file contains two functions which when used together can calculate the inverse of a square inversable matrix.
## The matrix and it's inverse are stored in the cache memory instead of the main memory to improve speed performance


## makeCacheMatrix function takes a matrix x and it creates a special object which can be cached, retrieved and overwritten. 
## The function allows to set(change) and get(fetch) the current value of the input matrix x and its inverse 
## to and from the cache 
## This function assumes that the input matrix x is square and inversable

makeCacheMatrix <- function(x = matrix()) {
  ## initialise inverse to make its environment to be that of the makeCacheMatrix function
  stored_inverse <- NULL
  
  ## allow the input matrix to be changed to a new one via the function set()
  set_matrix <- function (y = matrix()){
    x<<-y
    stored_inverse <- NULL ## make sure that the inverse we have stored in cache is removed
                     ##because it is no longer the inverse of the original input matrix
  }
  
  ## show which matrix is being inverted
  get_matrix<-function() {
    return (x)
  }
  
  ## update the cached value of the inverted matrix to be new_inverse matrix
  set_inverse<-function (new_inverse){
    stored_inverse<<-new_inverse
  }
  
  ## fetch the current stored inverse of the input matrix
  get_inverse <- function (){
    return (stored_inverse)
  }
  
  ## output of the function contains each of the listed functions
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve calculates the inverse of a matrix and passes it to cache memory instead of main memory
## cacheSolve uses makeCacheMatrix function to access cache
## input: x = a "special" matrix which is technically a list created by using function makeCacheMatrix
## output: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {  
  ## load the inverse of the matrix
  local_inverse <-x$get_inverse()
  
  if(!is.null(local_inverse)) {
    ## check if the inverse is already calculated and stored in cache.
    message("getting cached data")
    return(local_inverse)
  }
  else {
    ## if the inverse of the matrix is not calculated, calculate and store it in cache.
    local_data <- x$get_matrix()
    local_inverse <- solve(local_data, ...)
      
    x$set_inverse(local_inverse)
    return(local_inverse) 
  }   
    
}  
