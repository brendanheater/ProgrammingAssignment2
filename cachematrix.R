# Programming Assignment 2, Lexical Scoping
# Rather than include commentary throughou the function, 
# I include a line by line breakdown at the end of each function.

# This function calculates the inverse of a matrix, and stores it in Cache
makeCacheMatrix <- function(x = matrix()) {

    invmat      <- NULL
    
    setfunction <- function(y) { 
        x       <<- y
        invmat  <<- NULL
    }
    
    origmat     <- function() x
    setInvert   <- function(solve) invmat <<- solve
    getInvert   <- function() invmat
    list(set = setfunction, origmat = origmat,
         setInvert = setInvert, getInvert = getInvert)
        
}

# L6    , beginning of function.  Identifies that the function calls for a matrix.
# L8    , Set and clear the local variable 'invmat' within the function 'makeCacheMatrix'
# L10   , Establishes a function to store information in Cache
# L11   , Sets 'x' as a variable in the parent function to store information
# L12   , Sets 'invmat' as a variable in the parent function and defaults to NULL
# L15   , Creates a function which returns the matrix passed into the functon
# L16   , Creates a function which stores the 'solve' functionality
# L17   , Creates a function which returns the cached value of the inverted matrix
# L18/19, Place the contents of the function into a list which can be passed to a variable 
#         in the parent environment.


# This function compares a new matrix with a stored matrix.  If it is the same, it returns
# the stored inverse of the original matrix, otherwise it calculates a new inverse.  
cacheSolve <- function(x, ...) {
    
    premat <- x$getInvert()
    
    if(!is.null(premat)){
        
        message("This matrix is stored in cache.")
        message("Here is the inverse from cache.")
        return(premat)
        
    }
    
    else {
        
        message("This matrix is not stored in Cache.")
        message("Calculating inverse of matrix.")
        
        data <- x$origmat()
        premat <- solve(data, ...)
        x$setInvert(premat)
    }
                        
    premat
   
}
     
# L37   , Creates a function 'cacheSolve' which requires a variable 'x'
# L39   , Creates a variable 'premat' and stores a cached inverted matrix within
# L41   , Test whether 'NULL' is NOT stored in the inverted matrix cache, 
#         L43-L45 if TRUE, skip if FALSE
# L43/44, Print message to user to inform Cache use
# L45   , Return inverted matrix from Cache
# L49   , Second option from If Statement (L41), L51-L56
# L51/52, Print message to user to inform Matrix is not in Cache
# L54   , Pull cached original matrix, store as variable 'data'
# L55   , calculate the inverse of the original matrix and store as 'premat'
# L56   , Push L55 caclulation to cached value
# L59   , return inverted matrix to user