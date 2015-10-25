## Computing the inverse of a matrix
## In linear algebra, n-by-n square matrix A is called invertible if there exists an n-by-n square 
## matrix B such that A* Inv(A) = Inv(A)* A = I, where I denotes the n-by-n identity matrix.
## 
## Functions makeCacheMatrix() and cacheSolve() below make use of the scoping rules of the 
## R language to preserve the inverse of a matrix so that when we need it again, it can be 
## looked up in the cache rather than recomputed. In R, the inverse of a matrix can be obtained 
## with function solve().
##

## makeCacheMatrix()
## This function returns a list that holds the values of variable s, and functions set, get, 
## setsolve, and getsolve. 
##
##  Sample usage: 
## Given a variable gk defined as gk <- makeCacheMatrix( matrix (1:4,nrow=2,ncol=2)), the values of  
## s, set, get, setsolve and getsolve can be obtained by using: gk$s, gk$get(), gk$set(), or 
## gk$getsolve()
##

makeCacheMatrix <- function(x = matrix()) {

  ## s holds the inverse matrix value. Its value will remain 0 until cacheSolve is executed 
  ## the first time
  s <- NULL
  
  ## these initializations will determine the x and s values to be used in cacheSolve()
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x

  ## s will remain NULL until a value is assigned by cachesolve()
  setsolve <- function(solve) s <<- solve
  
  ## cacheSolve() has to be excuted in order to be able to obtain a value with gk$getsolve()
  getsolve <- function() s
  
  ## returns the list object
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve()
## This function calculates the inverse of the matrix, taking the values defined in  
## makeCacheMatrix(). The feature that enables performance improvement is the verification of the 
## the variable s that holds the inversed matrix. If it was calculated before, then the solve() 
## function is not executed.
##  Usage: inv <- cachesolve(gk)
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## s and x are always defined in the other function (makeCacheMatrix)
  
  ## Verifies whether the inverse has been executed before
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}