## Create 2 functions that will calculate the inverse of a matrix
## and cache the result to allow for faster computations

## The first function "makeCacheMatrix", creates a special "matrix",
## which contains the functions to get and set the values of the matrix
## and to get and store the inverse of a matrix
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)m<<-inverse
  getinv<-function()m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}
## The second function computes the inverse of a special matrix returned by
## "makeCacheMatrix". IF the inverse had been calculated (and the matrix
## has not changed), then the "cacheSolve" should retrieve the inverse from 
## cache.

cacheSolve<-function(x,...){
  ## checks for the inverse of the matrix and if it exists it returns the inverse
  ## matrix
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## if inverse did not exist it calculate the inverse using the solve function
  ## and sets this inverse matrix in the cache
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
}