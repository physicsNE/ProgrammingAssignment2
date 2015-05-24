## The first function, 'makeCacheMatrix' creates a vector of four functions as in the makeVector example.
## The functions are named setM, getM, setInv, and getInv. 
## We could have reused the makeVector function names, but adding M for Matrix makes the names more descriptive,
## as does changing setmean and getmean to setInv and getInv for the inverse matrices.
## 
## The second function 'cacheSolve' checks:
##     1) if there is an inverse matrix stored, 
##     2) if the stored matrix is the same matrix we want to Solve,
## If conditions 1 and 2 are both true, cacheSolve retrieves the inverse or solution matrix.

### Programmed by Michael McGuirk 5/21-24/2015
### We use two separate matrices: one to cache the inverse,  
###   the second to test that we do not use a stored inverse if the input matrix has changed.

testM<-array(c(2,3,4,5,0,7,8,23,10), dim=c(3,3))        ## det(testM) = 156
testMatrix<-array(c(2,3,4,5,0,7,8,9,10), dim=c(3,3))    ## det(testMatrix) = 72

## usage: cacheNAME<-makeCacheMatrix(x)
##        cacheNAME  is any name you choose to give the cache
##        x is the name of the matrix whose inverse you want cached
## prupose: makeCacheMatrix creates a list of four functions used to manipulate a cache 
##          that is a list (matrix, Inverse)

makeCacheMatrix <- function(x = matrix()) {      ## substitute the name of the matrix to cache for x
  Inv <- NULL                                    ## initialize the Inv (Inverse) to NULL
  setM <- function(x) {                          ## function call will be cacheNAME$set(y)
                                                 ##    where y is a matrix from any environment (global)
    x <<- x                                      ## creates a two element list(x,Inv) = (matrix,Inverse)
    Inv <<- solve(x)                             ## stores the list in the function environment, not global
  }
  getM <- function() x                           ## cacheNAME$getM retrieves the matrix from the function environment
  setInv <- function(Inv) Inv <<- solve(x)       ## cacheNAME$setInv stores Inv in the function envronment
  getInv <- function() Inv                       ## cacheNAME$getInv retrieve Inv from the function envronment
  list(setM = setM, getM = getM,                 ## places the four functions in a list  
       setInv = setInv,                          ##      so they can be accessed by the cacheSolve function
       getInv = getInv)
}


## usage: mInv<-cacheSolve(cacheNAME, mNAME)
## Return the inverse of matrix mNAME
##   from the cache if it is stored in cacheNAME
##   else compute the inverse, store the matrix and inverse in the cache, 
##        and return the inverse in mInv

cacheSolve <- function(cacheNAME, mNAME=mNAME) {
        
  Inv <- cacheNAME$getInv()                     ## retrieve the Inverse in the cache
  if(!is.null(Inv)) {                           ## test that an inverse is cached (is not NULL)
    message("getting cached data")
    data <- cacheNAME$getM()                    ## retrieve the cached matrix
    if (isTRUE(all.equal(data,mNAME))) return(Inv) else message("different matrix cached")                              ## if matrices are equal, return cached Inv
  }                                      
  message("caching new data")                   ## if not equal, update the cache
  cacheNAME$setM(mNAME)                         ##    cache the new matrix
  Inv <- solve(mNAME)                           ##    calculate its inverse
  cacheNAME$setInv(Inv)                         ##    cache the inverse
  return(Inv)                                   ## return the inverse of mNAME
}                                               ## prefer "return(Inv)" over just "Inv" for clarity

## testing the functions

cacheTEST1<-makeCacheMatrix(testM)              ## sets up the cache functions for matrix testM
mInv<-cacheSolve(cacheTEST1, testM)             ## caches testM and its inverse
print(mInv)                                     ## prints the inverse that it cached
mInv<-cacheSolve(cacheTEST1, testM)             ## finds the inverse was cached and returns it 
print("cached inverse")
print(mInv)                                     ## prints the inverse that it found
mInv<-cacheSolve(cacheTEST1, testMatrix)        ## changed vector doesn't return the old cached inverse
print(mInv)
## ends the test
