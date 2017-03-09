## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 

## Caching function for matrix argument. Returns a vector containing 
## getinverse to get the inverse of matrix and setinverse to set the inverse of the matrix.
## The function also provides a place holder for the matrix inverse calculated.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL 
        set <- function(y) {
                x <<-y 
                m <<- NULL
        } 
        
        get <- function () x 
        setinverse <-function(inverse) m<<- inverse 
        getinverse <-function() m 
        list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function  
                
## calculates the inverse of a square matrix, or fetches a 
## previously calculated inverse fron cache, given a list of arguments supplied by 
## the function makeCacheMatrix()                
                

cacheSolve <- function(x, ...) { 
         m<-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } 
        
        data <- x$get()
        m<-solve(data, ...)
        x$setinverse(m) 
        m
        ## Return a matrix that is the inverse of 'x'
}
