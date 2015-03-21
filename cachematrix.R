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

cacheSolve <- function(fnlist, ...) {  #fnlist is the list of functions returned by makeCacheMatrix
  
  mtx_inverse <- fnlist$get_inverse()  #call the get_inverse() function from fnlist
  
  if (!is.null(mtx_inverse)) {		#if the above call to get_inverse() results in a non-null value
    message("getting cached matrix inverse")
    return(mtx_inverse)			#then return that value
  }
  
  mtx <- fnlist$get_mtx()		#if we are still here, then the above call to get_inverse() returned a null
  mtx_inverse <- solve(mtx)		#call the solve() function to get the inverse of mtx
  fnlist$set_inverse(mtx_inverse)	#call set_inverse() with the just created inverse
  mtx_inverse				#this simply returns the matrix inverse
  
}
