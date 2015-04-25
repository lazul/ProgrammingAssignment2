## makeCacheMatrix serves as a function repository for caching Matrix and
## inverse Matrix, cacheSolve is a corresponding function that will check 
## for an inverse matrix if it is already calculated and stored in makeCacheMatrix
## and returns it. Else, it calculates it and returns it while also Caching it in 
## makeCacheMatrix.

## setting outer 'CacheMatrix' as outer environment that would allow
## for invMatrixand other variable cache

makeCacheMatrix <- function(x = matrix(),...)   ## x initialized as default empty (NA) matrix (...) to allow for matrix options
  ## initialize invMatrix variable to null
  invMatrix <- NULL
set <- function(mx) {
  ## set function allows a poss. of reassigning new data to matrix
  x <<- mx
}
get <- function() {
  ## returns x (matrix) in response to get
  x
}
setinv <- function(invm) {
  ## assigns a new value to invMatrix
  invMatrix <<- invm
}
getinv <- function() {
  ## calls (gets) value of invMatrix
  invMatrix
}
## puts functions into a list so they can be invoked
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##function to recall from special "matrix" inverse Matrix value if already
##cached. Else, function calculates invMatrix and chaches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getinv()
  ## checking if already cached
  if(!is.null(invMatrix)){
    message("getting inverse matrix")
    return(invMatrix)
  }
  ## getting matrix and calculating inverse Matrix
  matr <- x$get()
  invMatrix <- solve(matr)
  ## Caching 
  x$setinv(invMatrix)
  ## returning
  invMatrix
}