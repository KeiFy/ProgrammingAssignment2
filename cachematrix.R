## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
