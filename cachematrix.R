## Put comments here that give an overall description of what your
## functions do

## This function creates a cache of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m<<- solve
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)

}


## This functions returns the inverse of the cached matrix if the matrix is not changed. 
## ginv from MASS package can be used as a replacement to solve fn

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data if the matrix is not changes")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinv(m)
  m
  
}
## To Run:
##b<-makeCacheMatrix()
##b$set(matrix(c(0, 2, 1, 5,3,4,4,2,1), nrow = 3, ncol = 3, byrow = TRUE))
##cacheSolve(b)