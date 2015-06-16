## makeCachematrix creates a special "matrix" object that can cache its inverse.

## The special "matrix" object is a list containing functions to: set the matrix,get the matrix, set the inverse and get the inverse


makeCacheMatrix <- function(x = matrix()) {
  # x is a square invertible matrix
  #the operator <<- can be used to assign a value to an object
  #in an environment that is different from the current one.
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


#cacheSolve computes the inverse of the special "matrix" created with makeCacheMatrix. 
#It first check if the inverse has already been calculated.
#If so it gets the inverse from tha cache and skip the computation. 
#Otherwise, it calculates the inverse of the matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  #if the inverse has already been calculated then
  #retrieve data from the cache and skip computation.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    
  }
  #otherwise compute the inverse (use solve)
  data<-x$get()
  inv<-solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
