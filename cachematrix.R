## Put comments here that give an overall description of what your  functions do

#This function create a list of functions that can calculate the inverse of a matrix, stores it and recall the stored inverse
#for use instead of calculting it all over again, but will calculate the inverse if it has not been calculated before.


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

## 1. define a matrix via set(). In order to compute an inverse of a matrix
## one should make sure that matrix is square and not singular.
## 2. return a matrix via get() which was defined via set()
## 3. store an inverse of matrix via setinverse() at "inverse" variable
## 4. return an inverse of matrix via getinverse(), stored at "inverse" variable

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {      
          x <<- y
          inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
      getinverse <- function() inverse
          list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)  
}

## This function return a matrix that is the inverse of matrix which was set
## via makeCacheMatrix().In order to compute an inverse of a matrix
## one should make sure that matrix is square and not singular. cacheSolve() don't
## check for this. To prevent errors one can use a "matrixcalc" library, 
## is.square.matrix() and is.non.singular.matrix() functions.
## cacheSolve() first check if the inverse of matrix was already computed.
## if not, it compute it via solve() and call setinverse() to reassign the
## "inverse" variable at makeCacheMatrix()  

cacheSolve <- function(x) {
      inverse <- x$getinverse()
          if(!is.null(inverse)) {
            message("getting cached inverse matrix")
          return(inverse)
          }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}

