## The following is the code for a pair of functions that can cache the inverse
## of a matrix


## The first function: makeCacheMatrix is used to create a special "matrix" object
## that can cache its inverse
## This function essentially creates a list of matrix functions called
## "setmat", "getmat", "savematinv" and "getmatinv" respectively

makeCacheMatrix <- function(xmat = matrix()) {        
        ## here, "xmat" is the input to the function which is a matrix
        
        ## use the variable "minv" as the cache for the inverse of xmat
        ## first, initialize the cache to NULL since it has not been calculated yet
        minv <- NULL
        
        ## next, define a function "setmat" to set the value of "xmat" in the 
        ## "makeCacheMatrix" function environment
        setmat <- function(ymat) {
                xmat <<- ymat
                minv <<- NULL
        }
        
        ## next, define a function "getmat" which will return the value of "xmat"
        ## ONLY after it has been set using "setmat" of course
        
        getmat <- function() xmat
        
        ## next, define a function "savematinv" which takes a matrix passed to
        ## it and stores in it the cache i.e. "minv"
        
        savematinv <- function(cacheinv) minv <<- cacheinv
        
        ## next, define a function "getmatinv" which essentially returns the 
        ## value of the stored "minv" i.e. cached matrix inverse
        
        getmatinv <- function() minv
        
        ## the actual output of the "makeCacheMatrix" function is a list of the
        ## above matrix functions, any of which can be called as needed to set or get
        ## the matrix or its inverse
        
        list(   setmat = setmat, 
                getmat = getmat,
                savematinv = savematinv,
                getmatinv = getmatinv )
}

## following this, we come to the second main function which is the actual
## "cacheSolve" function used to calculate the matrix inverse
## here we use the "solve" function implemented in R which can be directly used
## for matrix inversion

cacheSolve <- function(xmat, ...) {
        
        ## first the function checks if the cached inverse matrix already exists
        ## by calling "getmatinv" on the passed matrix object
    
        minv <- xmat$getmatinv()
        
        ## if the cached inverse already exists i.e. is NOT NULL, the cached inverse
        ## is returned directly along with the message below
        
        if(!is.null(minv)) {
                message("getting cached inverse matrix")
                return(minv)
        }
        
        ## if the cached inverse has not been calculated yet, the cached matrix
        ## is passed to the local "data" variable and the "solve" function is used
        ## to a) get the inverse and b) save this in the cache by calling the 
        ## "savematinv" function. Finally the inverse is reported.
        
        data <- xmat$getmat()
        
        ## in the "solve" function we do not pass it a 2nd matrix argument
        ## which implies that the inverse of the input matrix is returned
        
        minv <- solve(data) 
        
        ## saved the calculated inverse in the cache
        xmat$savematinv(minv)
        
        ## report the inverted matrix
        minv
}