## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mtx = matrix()) {  #mtx constitutes the data
  
  mtx_inverse <- NULL  		#initialize the matrix inverse to NULL
  
  set_mtx <- function(new_mtx) {		#set_mtx function is called with argument new_mtx
    mtx <<- new_mtx
    mtx_inverse <<- NULL
  } 
  
  get_mtx <- function() mtx		#get_mtx() simply returns the current data (mtx)
  
  set_inverse <- function(new_inverse) mtx_inverse <<- new_inverse  #set the inverse to the argument: new_inverse
  
  get_inverse <- function() mtx_inverse   #simply return the current inverse (mtx_inverse)
  
  list(
    set_mtx = set_mtx, 
    get_mtx = get_mtx,
    set_inverse = set_inverse,
    get_inverse = get_inverse
  )
  
}


## Write a short comment describing this function

cacheSolve <- function(fnlist, ...) {
  
  mtx_inverse <- fnlist$get_inverse()
  
  if (!is.null(mtx_inverse)) {
    message("getting cached matrix inverse")
    return(mtx_inverse)
  }
  
  mtx <- fnlist$get_mtx()
  mtx_inverse <- solve(mtx)
  fnlist$set_inverse(mtx_inverse)
  mtx_inverse
  
}
