makeCacheMatrix<- function(x = numeric()) { #Creates a list which stores 4 values for a given matrix, which include the matrix itself, as well as the inverse matrix
  m <- NULL # Sets "m" to "NULL"
  set <- function(y) { #When called, stores a new matrix in value "x"
    x <<- y
    m <<- NULL
  }
  get <- function() x # When called, returns the original matrix which makeCacheMatrix was called upon
  solvmat <- function(solve) m <<- solve #When called, calculates and returns the inverse matrix
  getinverse <- function() m # When called, returns the inverse matrix, if already computed
  list(set = set, get = get, #Last evaluation inside the function, makes the output of the function a list of functions
       solvmat = solvmat,
       getinverse= getinverse)
}

cacheSolve <- function(x, ...) { # This function retrieves the inverse of the matrix on which makeCachMatrix was run on. If theinverse hasnot been calculated, it calculates the inverse matrix and caches it
  m <- x$getinverse() # Runs getinverse from above function
  if(!is.null(m)) { # If inverse matri hasben previously calculated, then...
    message("getting cached data") 
    return(m) # Returns said inverse matrix
  }
  data <- x$get() # If it hadnot been previously calculated, uses the get function above to retrieve the original matrix
  m <- solve(data, ...) # Calculates the inverse matrix
  x$solvmat(m) # Caches the result
  m # Evaluates m, which stores the inverted matrix, an since it is the last evaluation will be the output of this function
}
