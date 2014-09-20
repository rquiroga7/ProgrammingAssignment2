makeCacheMatrix<- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  solvmat <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       solvmat = solvmat,
       getinverse= getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$solvmat(m)
  m
}
