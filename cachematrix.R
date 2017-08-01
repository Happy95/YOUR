## Computation of a matrix inverse is a time consuming process.So,caching the inverse of
## the matrix saves a lot of time. Such is the code below which solves the inverse and
## caches it every time it is computed.

## This function does four operations for the matrix caching.The functions below:
## 1.Set the matrix data 2.Get the matrix data 
## 3.Set the inverse of matrix 4.Get the inverse of the matrix


makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y){
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves the inverse of the above created matrix.
## For this it first checks whether the matrix inverse has been calculated or not.
## If done it returns the cache of the inverse of the matrix and if not then
## it solves the inverse of the matrix and returns the same. 

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data)
  m$setinverse(inv)
  inv
}
