## Put comments here that give an overall description of what your
## functions do

## Create a "matrix" that is a list containing functions:
## 1. 'set(y)' sets the value of the "matrix" to y
## 2. 'get()' returns the value of the "matrix"
## 3. 'setinverse(z)' sets the "inverse" to z
## 4. 'getinverse()' returns the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
    set <- function(y) {
        x<<-y
        inverse<<-NULL
    }
    get <- function() x
    setinverse<-function(inv) inverse<<-inv
    getinverse<-function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns a matrix that is the inverse of 'x'
## If the inverse already exists ('inverse' is not null)
## then return value of inverse
## Otherwise calculate the inverse of 'x' and set the value of 'inverse'
## return the value of inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <-x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinverse(inv)
}
        
