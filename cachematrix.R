## makeCacheMatrix returns a list containing functions to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of inverse of the matrix
## 4) get the value of inverse of the matrix
## These functions are encapsulated in a list.

## This function, makeCacheMatrix Creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL ##  Intialization: set to NULL
  
  ## This function sets the matrix and changes when we give the value.
  set <- function(y) {
    x <<- y 
    inverse.matrix <<- NULL
  }
  ## This function gets the matrix itself (x)
  get <- function() x 
  setInverse<- function(inverse) inverse.matrix <<- inverse ## Sets the inverse of the matrix
  getInverse <- function() inverse.matrix ## gets  the inverse matrix
  
  list(set = set,
       get = get,
       setInverse= setInverse,
       getInverse = getInverse) ## This list is used as an input to the cacheSolve() function
}


## The cacheSolve function returns the inverse matrix. The matrix is already created by the function makeCacheMatrix! 
## We have two Conditions:- 
##	1) cacheSolve returns from the cache if the inverse matrix calculation has already done and no change is applied to it.
## 2) cacheSolve returns the result by doing the computation.  
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## The input x is the output of the matrix makeCacheMatrix()
  
  inverse.matrix <- x$getInverse() ## Retrieves the cached value for the inverse matrix
  
  #  Checks  if the inverse has already been calculated/computed. 
  if(!is.null(inverse.matrix)) {
    # Then return the cached value ---- It means no computation will takes place.		
    message("Getting cached matrix")
    return(inverse.matrix)
  }
  
  # Otherwise, it calculates the inverse matrix.
  matrix <- x$get()
  
  inverse.matrix <- solve(matrix, ...)
  
  # Cache this result in the object
  x$setInverse(inverse.matrix)
  
  # This retrieves the  computed value 
  inverse.matrix  
}

 
 ## Examples for testing my functions!
 ## x <- matrix(4:7, 2, 2)
 ## m <- makeCacheMatrix(x)

## > m$get()
##      [,1] [,2]
## [1,]    4    6
## [2,]    5    7
## > cacheSolve(m)  --- At first values were not cached. The second run will retrieve values from the cache.
##	     [,1] [,2]
##	[1,] -3.5    3
##	[2,]  2.5   -2

## > cacheSolve(m)   --- Here cacheSolve(m) function is running for the 2nd time. So returns cached value.
## 	Getting cached matrix
##	     [,1] [,2]
##	[1,] -3.5    3
##	[2,]  2.5   -2

