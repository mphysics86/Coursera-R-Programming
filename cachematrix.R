## R Programming
## Assignment 2
## 
## Contains two functions that compute and cache the inverse of a matrix
## To use, call the "makeCacheMatrix()" function first to transform current matrix into a special "matrix" object
## Then call "cacheSolve()" to store and retrive the inverse

## Takes a matrix argument and creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set<- function(y){
            
            x <<- y
            i <<- NULL
      }
      
      get<- function() x
      
      setinv <- function(inv) i <<- inv
      
      getinv <- function() i
      
      list(set=set, get=get, 
           setinv = setinv, 
           getinv = getinv)
      
}


## Returns the inverse of a special "matrix" object.
## Checks for inverse in cache first. Returns value if found
## Otherwise, calculates and caches the inverse if it has not yet been cached
## 

cacheSolve <- function(x, ...) {

            i <- x$getinv()
            if(!is.null(i)) {
                  message("getting cached data")
                  return(i)
            }
            
            data <- x$get()
            i <- solve(data, ...)
            x$setinv(i)
            i
      }
