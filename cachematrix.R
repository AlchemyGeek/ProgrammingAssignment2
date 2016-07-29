## CacheMatrix caches the result of 
## a matrix operation (inverse or other)

# Constructor for CacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  ## Cached matrix value
  mcached <- NULL
  
  ## Set the initial matrix value and initialize cache
  set <- function(y) {
    x <<- y
    mcached <<- NULL
  } 
  
  ## Return the matrix value
  get <- function() x
  
  ## Set cache value
  setcache <- function(calc) mcached <<- calc
  getcache <- function() mcached
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## Calculate Matrix Inverse operation
## Return either cached value or new calculation
cacheSolve <- function(x, ...) {
  
  cache <- x$getcache()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }

  ## Retrieve matrix
  data <- x$get()
  
  ## Calculate Inverse
  cache <- solve(data, ...)
  
  ## Set cache to inv matrix
  x$setcache(cache)
  
  ## Return a matrix that is the inverse of 'x'
  cache
}


