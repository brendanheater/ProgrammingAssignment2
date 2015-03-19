## Programming Assignment 2, Lexical Scoping
## Throughout the program, I will include light annotations.  At the end of each function, I include a line by line breakdown.

## This function calculates the inverse of a matrix, and stores it in Cache

makeCacheMatrix <- function(x = matrix()) {

    invmat <- solve(x)
    
    setfunction <- function(y) { 
        x      <<- y
        invmat <<- NULL
    }
    
    origmat   <- function() x
    setInvert <- function(solve) invmat <<- solve
    getInvert <- function() invmat
    list(set = setfunction, origmat = origmat,
         setInvert = setInvert, getInvert = getInvert)
    
    
}


## This function compares a new matrix with a stored matrix.  If it is the same, it returns
## the stored inverse of the original matrix, otherwise it calculates a new inverse.

cacheSolve <- function(z, ...) {
    
    if(identical(z,x$origmat())){
        
        message("This matrix is stored in cache.")
        message("Here is the inverse from cache.")
        return(x$getInvert())
        
    }
    
    else {
        
        message("This matrix is different than the stored matrix.")
        message("Calculating inverse of new matrix.")
        matNew <- solve(z)
        return(matNew)
        
    }
    
    
}
