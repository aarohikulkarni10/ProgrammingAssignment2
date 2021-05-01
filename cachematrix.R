##The following program for an input of matrix gives the inverse of it. The speciality is that it caches the matrix so it
##saves the efforts of computing it again.


##This function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix<-function(x=matrix()){
inv<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function() {x}
setInv<-function(inverse) {inv<<-inverse}
getInv<-function() {inv}
list(set=set, get=get, setInv=setInv, getInv=getInv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve<- function(x,...){
  inv <- x$getInv()
  if(!is.null(inv)){
    message("This is cached data.....")
    return(inv)
    
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInv(inv)
  inv 
}

##End of the code 
##AAROHIK

