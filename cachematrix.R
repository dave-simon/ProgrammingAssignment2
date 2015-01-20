## These two functions were created in accordance with Programming Assingmnet # 2, 
# which is to write a pair of functions that cache the inverse of a given invertible matrix 
# (which must be a square one, like 2 by 2 or 3 by 3).

# Function makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   
  
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 

}


#Function cacheSolve:
#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then cacheSolve should retrieve the
#inverse from the cache and print a "getting cached data" message.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data.")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
  inverseMatrix

}



