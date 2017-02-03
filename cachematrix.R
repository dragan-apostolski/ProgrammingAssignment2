## Both functions are working together such that makeCacheMatrix recives a matrix x as an argument, 
## and the inverse matrix is initialized via the cacheSolve function.


## This function hold one boolean flag which is initially set to false, which means that the inverse matrix for the argument x is not set.
## It has inner functions for setting the inversed matrix and getting the flag. Once the setInversed function has been called, x is set to be
## the new inversed matrix and the flag now turns to TRUE, which indicates that the inverse matrix has been set for x.
## The return list hold references to the functions for manipulating the argument matrix x.

makeCacheMatrix <- function(x = matrix()) {
      flag <- FALSE
      setInversed <- function(y = matrix()){
            x <<- y
            flag <<- TRUE
      }
      getFlag <- function() flag
      getMatrix <- function() x
      list(flag = getFlag, set = setInversed, get = getMatrix)
}


## The cacheSolve function receives a list x, which hold references to the functions for manipulating the matrix which was made
## by the makeCacheMatrix function. The flag tells whether previously the matrix was inversed or not. If it wasn't, this functions find
## the inverse matrix and sets it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      f <- x$flag()
      if(f){
            x
      }
      m <- x$get()
      m <- solve(m)
      x$set(m)
      m
}