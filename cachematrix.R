## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL        
  set <- function(y){           #sets the value of the matrice
    x <<- y                      
    inv <<- NULL
  }
  
  get <- function() x           #gets the value of matrice
  
  setinverse <- function(inverse) inv <<-inverse  #set inverse of the matrice
  getinverse <- function() inv                    #get inverse of the matrice
  list(set = set, get = get, setinverse= setinverse, getinverse= getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()           #get inverse of matrice from cache
  if(!is.null(inv)) {             #if value is there ib the cache then return cached value
    message("getting cached data")
    return(inv)
   }
    data <- x$get()
    inv <- solve(data, ...)     # if cahced value is not present solve for inverse 
    x$setinverse(inv)
    inv
}
