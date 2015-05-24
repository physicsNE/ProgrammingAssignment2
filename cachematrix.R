## The first function, 'makeCacheMatrix' creates a vector of four functions as in the makeVector example.
## The functions are named setM, getM, setInv, and getInv. 
## We could have used the same names, but adding M for Matrix makes the names more descriptive,
## as does changing setmean and getmean to setInv and getInv for the inverse matrices.
## functions do

### Programmed by Michael McGuirk 5/21/2015
### We use two separate matrices: one to cache the inverse,  
###   the second to test that we do not use a stored inverse if the input matrix has changed.

testM<-array(c(2,3,4,5,0,7,8,23,10), dim=c(3,3))        ## det(testM) = 156
testMatrix<-array(c(2,3,4,5,0,7,8,9,10), dim=c(3,3))    ## det(testMatrix) = 72

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {             ## substitute the name of the matrix to cache for x
  Inv <- NULL                                           ## start with NULL as the Inv (Inverse)
  setM <- function(y) {                                 
    x <<- y
    Inv <<- solve(y)
  }
  getM <- function() x
  setInv <- function(Inv) Inv <<- solve(y)
  getInv <- function() Inv
  list(setM = setM, getM = getM,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
