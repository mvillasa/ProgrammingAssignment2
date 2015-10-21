## These sets of functions are used to compute or retrive the stored 
## value of the inverse of a matrix.  If the inverse of a matrix has already
## been computed, then it will be stored in the special matrix object
## created by the function makeCacheMatrix.  If it has not been computed, then 
## upon computation it will be stored in the special matrix object for further use.
## An example of usage is the following:
## 
## #create a 2x2 invertable matrix
## c=rbind(c(1, -1/4), c(-1/4, 1)) 
##
## #Create the special matrix structure. Initially no inverse has been computed. 
## CM<-makeCacheMatrix(c)
##
## #upon the first call of the function cacheSolve, the coputed inverse will be stored.
## cacheSolve(CM)
##          [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##
## #succesive calls on the function will retrieve the computed and stored inverse.
## cacheSolve(CM)
## getting cached data
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##
## October 2015

## The function makeCacheMatrix will create a structure that stores
## the inverse value of a matrix and the functions that store and retrive
## data from the structure.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function will return the inverse of the matrix x 
## if the inverse exists in the new matrix strcuture created by makeCacheMatrix
## then it will return the 'chached' value, otherwise it will compute
## the inverse using solve.
 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    matriz <- x$get()
    inv <- solve(matriz, ...)
    x$setinverse(inv)
    inv
}

