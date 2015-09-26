## Create a matrix with a cacheable inverse via getters/setters

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL #stub for inverse matrix
  
  #getters
  get <- function() x
  getinv <- function() inv
  
  #setters
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  setinv <- function(invmx) inv <<- invmx
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## calculate the mx inverse or get it from cache and return

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  
  inv <- x$getinv() #try getting the cached inverse
  
  if(!is.null(inv)) { #avail in cache
    message("getting inverse matrix from cache")
    return(inv)   #function exits
  }
  
  #not cached - do it the hard way  
  data <- x$get()
  inv <- solve(data, ...) #throws errors when fed with non-square
  x$setinv(inv) #save to cache
  inv
}
