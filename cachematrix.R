# The function makeCacheMatrix creates a matrix object with a list of functions 
# (set,get,setInverse and getInverse) which can be used to access and modify its data.
# The matrix object can hold the actual matrix and cache a copy of the computed inverse.
# The function cacheSolve accesses the cached inverse
# and computes and caches it if is not already computed.



makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates a matrix object. It has the following data:
  # x contains the matrix itself. On creation i.e. when makeCacheMatrix is called,
  # it is copied from the argument x.
  # inv can contain the inverse.
  # On creation, inv is set to NULL and stays NULL until the inverse has been computed.
  inv <- NULL
  set <- function(y) {
    # The data of the matrix object are modified by the set function.
    # The matrix itself is copied from the argument y:
    x <<- y
    # inv is reset to NULL to avoid that the wrong inverse is returned, 
    # in case it has been computed for the old data.
    inv <<- NULL
  }
  get <- function() {
    # The get function returns the data, i.e. the actual matrix x.
    return(x)}
  setInverse <- function(Inverse) {
    # The setInverse function stores the value of the computed inverse provided in 
    # the argument Inverse in the variable inv.
    return (inv <<- Inverse)}
  getInverse <- function() {
    # The getInverse function returns the value of inv. If setInverse hasn't been called yet,
    # getInverse will return NULL.
    return(inv)}
  # A list with the four functions is returned. 
  # Each list returned by makeCacheMatrix will have its own environment and therefore its own 
  # instances of x and inv.
  return(list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse))
  
}

cacheSolve <- function(M, ...) {
  # cacheSolve Return a matrix that is the inverse of the matrix contained in M.
  # The value of inv belonging to the matrix object M is retrieved.
  inv_stored <- M$getInverse()
  if(!is.null(inv_stored)) {
    #  If the value of inv_stored is not NULL then the inverse has already been computed so 
    #  the message "getting cached data" is displayed and inv_stored is returned.
    message("getting cached data")
    return(inv_stored)
  }
  # If the value of inv_stored is  NULL then the inverse has not yet been computed,
  # so the following steps are undertaken to compute it.
  # The value of x belonging to the matrix object M is retrieved.
  data <- M$get()
  # The inverse of x is computed.
  inv_computed <- solve(data, ...)
  # With the function setInverse, the value of inv belonging to the matrix object M
  # is set to inv_computed.
  M$setInverse(inv_computed)
  # The newly computed inverse is returned.
  return(inv_computed)
}
