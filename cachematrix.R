## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to :set the value of the matrix;get the value of the matrix;set 
## the value of the inverse;get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- matrix(NA,nrow(x),ncol(x))
        set <- function(y){
                x<<-y
                s<<-matrix(NA,nrow(x),ncol(x))             
        }
        get <- function()x
        setinverse <- function(solve) s<<-solve
        getinverse <- function() s
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "vector" created with the 
## above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse in the cache via the 
## setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.na(s[1,1])){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setinverse(s)
        s
}
