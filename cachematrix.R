## This pair of functions creates and caches a matrix or matrices,
## to be restored and inverted at a later time. It can store as many
## matrices as needed.

## This is a variant on the makeVector function provided as an example
## with the assignment.
## The matrix must be square (same number of rows as columns), but
## that's a requirement for it to be invertable at all and not a bug.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(s) m <<- s
  getinv <- function() m
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This inverts the matrix given, or gets the cached matrix and
## inverts it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Rooting through cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
