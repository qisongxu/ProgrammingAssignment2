## In this assignment cachematrix.R, two functions has been created to cache the
## the inverse of a matrix.
## Part (A): Function makeCacheMatrix creates a special "matrix" that can cache
## its inverse.
## Part (B): Function cacheSolve calculates the inverse of the special "matrix"
## returned by function makeCacheMatrix. If inverse matrix has been computed
## (for a given matrix), then cachSolve will retrieve the inverse from the cache.

## Part (A): The first function, makeCacheMatrix, creates a special "matrix",
## which is a list containing four functions performing different tasks:
## Input:   Variable x is a square matrix
## Output:  Special "matrix" - a list of containing the following four functions:
## (1) set_matrix: set the value of matrix
## (2) get_matrix: get the value of matrix
## (3) set_inv_matrix: set the value of inverse matrix
## (4) get_inv_matrix: get the value of inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        #!! Check that the input matrix is square. If not, function will be
        #!! stopped and an error message will be output to user.
        if(nrow(x)!= ncol(x)) {
           stop("Input matrix is not square! User, please enter a square matrix")
        }
        #!! Check that the input matrix x is invertible. If not, function will be
        #!! stopped and an error message will be output to user.
        if(det(x)== 0) {
           stop("Input matrix is non-invertible! 
                 User, please enter a square matrix with non-zero determinant.")
        }
        # (0) Inverse matrix x is set to NULL.
        inv_x <- NULL
        # (1) Function set_matrix allows cache variable x to store 
        # the input matrix y. The cached variable inv_x will store NULL.
        set_matrix <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        # (2) Function get_matrix recalls/prints the cache matrix x
        get_matrix <- function() x
        # (3) Function set_inv_matrix takes in the computed inverse matrix x
        # and store the value in the cache variable inv_x
        set_inv_matrix <- function(inv) inv_x <<- inv
        # (4) Function get_inv_matrix recalls/prints cache inverse matrix inv_x
        get_inv_matrix <- function() inv_x
        # The final output of the function makeCacheMatrix is the list 
        # containing all the four functions as described above.
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}


## Part (B): The second function, cacheSolve, will output the inverse matrix of
## the special "matrix" created from the first function, makeCacheMatrix.
## Input:       Variable spec_mat is the special "matrix" 
## Output:      The inverse matrix of spec_mat
cacheSolve <- function(spec_mat, ...) {
        ## (1) First part of function cacheSolve: Check if the inverse matrix
        ## has already been calculated. If the inverse matrix has been computed,
        ## its value will be returned immediately without further computation.
        inv <- spec_mat$get_inv_matrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## (2) Second part of function cacheSolve: Compute the inverse matrix.
        ## The value of the matrix will retrieve by get_matrix function and 
        ## solve for its inverse matrix. The result will be returned and stored
        ## in cache via the set_inv_matrix function.
        x <- spec_mat$get_matrix()       
        inv <- solve(x, ...)
        spec_mat$set_inv_matrix(inv)
        inv
}
