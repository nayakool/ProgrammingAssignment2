##Objective of this program is to make use of caching methodology to avoid redunent calls
##made to comupte intensive tasks like inversion of a matrix

## makeCacheMatrix defines list of getter and setter functions to get and set the Matrix 
## and the Inverse of that matrix 

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInverseMatrix <- function(InverseMatrix) m <<- InverseMatrix
        getInverseMatrix <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## cacheSolve computes of the inverse of the Matrix created by the function above but d
##does a check to see if the inverse of the matrix is already cached, if yes it returns the inverse matrix
##from cached object else it computes the inverse of the matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data)
        x$setMatrix(m)
        m
}
