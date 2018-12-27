##################################################################
## Pair of functions that will return the inverse of a matrix
## while caching the solved matrix to avoid recalculating it
## on future calls.

## The first function takes a matrix as an argument and creates
## a special object that should be used as an argument on the
## second function, to return its inverse matrix.

## USAGE EXAMPLE: ###############################################
## This will create a special MakeCacheMatrix Object:
## > OrigMatrix <- matrix(c(1:4), nrow=2, ncol=2, byrow = TRUE)
## > SpecialMatrixObject <- makeCacheMatrix(OrigMatrix)
##
## this will solve the matrix store in  the special object:
## > cacheSolve(SpecialMatrixObject)
## Another call with the same arguments will retrieve the
## already solved matrix, instead of recalculating it:
## > cacheSolve(SpecialMatrixObject)


## MakeCacheMatrix Function ####################################
## This function will take a matrix as an argument store it and
## empty any previously calculated matrix, if any.

makeCacheMatrix <- function(x = matrix()) {
  CalculatedInverse <- NULL
  
  set <- function(y) {
    x <<- y
    CalculatedInverse <<- NULL
  }
  get <- function() {x}
  SetInvMatrix <- function(SolvedMatrix) CalculatedInverse <<- SolvedMatrix
  GetInvMatrix <- function() CalculatedInverse
  
  list( set = set, get = get,
        SetInvMatrix = SetInvMatrix,
        GetInvMatrix = GetInvMatrix
  )
}

## The CacheSolve function #####################################
## this function will return the inverse matrix of the original
## matrix. The function will first check if the matrix has been
## already calculated. If so, it will retrieve the calculated
## matrix from the special object and then return it without any
## further calculation. If not, the function will retrieve the
## original matrix, solve it, store it on the special matrix
## object (to avoid recalculating on future calls) and, finally,
## return the solved matrix.

cacheSolve <- function(x, ...) {
  CalculatedInverse <- x$GetInvMatrix()
  if(!is.null(CalculatedInverse)) {
    message("getting cached matrix")
    return(CalculatedInverse)
  }
  data <- x$get()
  CalculatedInverse <- solve(data, ...)
  x$SetInvMatrix(CalculatedInverse)
  CalculatedInverse
}