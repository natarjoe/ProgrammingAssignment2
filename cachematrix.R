## The purpose of the 2 functions below is to take advantage of the different environments available
## in R to cache results obtained from lengthy  calculations so we dont have to repeat them over again 
## 

## The makeCacheMatrix returns a list of four functions that set and get the matrix which is passed
## as a parameter and set and get inverse of the matrix that has been passed to the function
##

makeCacheMatrix <- function(x = matrix()) {
  
  mi <-NULL
  set <-function(y) {
    x<<-y
    mi <<-NULL
    
  }
  
  get <-function (){
    x
    }
  
  setInverse <- function(Inv) {
    mi <<- Inv                     ## set the value of inverse of martrix in parent environment of this funcion
    }
  getInverse <-function(){
    mi
  }
  
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}
  
  




## This function returns the value of the inverse matrix. It first  checks the cache and if the 
## inverse is already calculated it returns it . Else it calculates the inverse and sets the value in 
## the parent envitronment.

cacheSolve <- function(x, ...) {
        
  mi<-x$getInverse()
  if(!is.null(mi)){
    
    message("getting cached data")
    return (mi)
    
  }
  
  mat<-x$get()
  mi<-solve(mat,...) ## calculate inverse
  x$setInverse(mi)
  mi
  
}
