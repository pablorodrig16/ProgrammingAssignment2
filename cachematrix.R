## These functions calculate the inverse matrix resulting from
## applying solve() function to an square matrix and store both of them in a list.
## Then if that inverse matrix is needed it can be 
## get from de cache without needing to perform calculations again.


## 'makeCacheMatrix' function creates a list containing 4 functions: 
## 1.     $set(newmatrix): sets a matrix to be used. This function allows to 
##      easily assign a new matrix to a previously created list for calculation.
## 2.     $get() gets the original matrix
## 3.     $setSolve(inverse) sets the inverse matrix in the cache
## 4.     $getSolve () gets the inverse matrix from the cache
## 
## 'x' must be a square and invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        solvematrix <- NULL
        set <- function(newmatrix) {
                x <<- newmatrix
                solvematrix <<- NULL
        }
        get <- function() x
        setSolve <- function(inverse) solvematrix <<- inverse
        getSolve <- function() solvematrix
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## 'cacheSolve' returns the inverse matrix from the original one. When it is called 
## for the first time it return the result from applying solve() function to the 
## original matrix and set this result on the cache. Therefore when cacheSolve()
## is called it gets the inverse matrix from the cache.
## 'x' argument must be a list created with 'makeCacheMatrix' function containing
## the original matrix.

cacheSolve <- function(x, ...) {

        inverse <- x$getSolve()
        if(!is.null(inverse)) {
                message("getting cached matrix")
                return(inverse)
        }else{
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setSolve(inverse)
        inverse
        }
}
